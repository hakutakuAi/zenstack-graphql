import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler, GenerationError } from '@utils/error-handler'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'
import { SchemaBuilder } from '@utils/schema-builder'
import { CoreGeneratorContext, DMMF } from '@types'
import { NormalizedOptions } from '@utils/options-validator'
import { GeneratorFactory } from '@utils/generator-factory'
import { TypeRegistry, TypeKind, TypeInfo } from '@utils/type-registry'
import { match } from 'ts-pattern'
import { ScalarGenerator } from '@generators/scalar-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'

export interface GenerationStats {
	objectTypes: number
	enumTypes: number
	scalarTypes: number
	relationFields: number
	connectionTypes: number
	warnings: string[]
}

export interface GenerationResult {
	sdl: string
	stats: GenerationStats
}

export class CoreGenerator {
	private schemaComposer: SchemaComposer<unknown>
	private errorHandler: ErrorHandler
	private attributeProcessor: AttributeProcessor
	private typeMapper: TypeMapper
	private options: NormalizedOptions
	private dmmf: DMMF.Document
	private schemaBuilder: SchemaBuilder
	private warnings: string[] = []
	private generatorFactory: GeneratorFactory
	private typeRegistry: TypeRegistry

	constructor(context: CoreGeneratorContext) {
		this.options = context.options
		this.dmmf = context.dmmf
		this.errorHandler = context.errorHandler || ErrorHandler.getInstance()
		this.attributeProcessor = context.attributeProcessor || new AttributeProcessor()
		this.typeMapper = context.typeMapper || TypeMapper.createFromDMMF(context.dmmf)
		this.schemaComposer = context.schemaComposer || new SchemaComposer()
		this.schemaBuilder = new SchemaBuilder(this.schemaComposer, this.errorHandler)
		this.typeRegistry = new TypeRegistry(this.schemaComposer, this.errorHandler)

		this.generatorFactory = new GeneratorFactory({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			dmmfModels: this.dmmf.datamodel.models,
			dmmfEnums: this.dmmf.datamodel.enums,
		})
	}

	generateSchema(): GenerationResult {
		try {
			if (this.options.relayCompliant) {
				this.schemaBuilder.addRelayRequirements()
			}

			const scalarGenerator = this.createScalarGenerator()
			scalarGenerator.generate()
			const scalarTypes = scalarGenerator.getGeneratedScalars()

			scalarTypes.forEach((typeName) => {
				const composer = this.schemaComposer.getSTC(typeName)
				if (composer) {
					this.typeRegistry.registerType(typeName, TypeKind.SCALAR, composer, true)
				}
			})

			const enumGenerator = this.createEnumGenerator()
			enumGenerator.generate()
			const enumTypes = enumGenerator.getGeneratedEnums()

			enumTypes.forEach((typeName) => {
				const composer = this.schemaComposer.getETC(typeName)
				if (composer) {
					this.typeRegistry.registerType(typeName, TypeKind.ENUM, composer, true)
				}
			})

			const objectGenerator = this.createObjectTypeGenerator()
			objectGenerator.generate()
			const objectTypes = objectGenerator.getGeneratedObjectTypes()

			objectTypes.forEach((typeName) => {
				const composer = this.schemaComposer.getOTC(typeName)
				if (composer) {
					this.typeRegistry.registerType(typeName, TypeKind.OBJECT, composer, true)
				}
			})

			const relationGenerator = this.createRelationGenerator()
			relationGenerator.generate()
			const relationFields = relationGenerator.getGeneratedRelations()

			let connectionTypes: string[] = []
			if (this.options.connectionTypes) {
				const connectionGenerator = this.createConnectionGenerator()
				connectionGenerator.generate()
				connectionTypes = connectionGenerator.getGeneratedConnectionTypes()

				connectionTypes.forEach((typeName) => {
					const composer = this.schemaComposer.getOTC(typeName)
					if (composer) {
						this.typeRegistry.registerType(typeName, TypeKind.CONNECTION, composer, true)
					}
				})

				const edgeTypes = connectionGenerator.getGeneratedEdgeTypes()
				edgeTypes.forEach((typeName) => {
					const composer = this.schemaComposer.getOTC(typeName)
					if (composer) {
						this.typeRegistry.registerType(typeName, TypeKind.EDGE, composer, true)
					}
				})
			}

			const validationErrors = this.schemaBuilder.validateSchema()
			if (validationErrors.length > 0) {
				this.warnings.push(...validationErrors)
			}

			this.validateTypesWithRegistry()

			const sdl = this.schemaBuilder.generateSDL()

			return {
				sdl,
				stats: {
					objectTypes: objectTypes.length,
					enumTypes: enumTypes.length,
					scalarTypes: scalarTypes.length,
					relationFields: relationFields.length,
					connectionTypes: connectionTypes.length,
					warnings: this.warnings,
				},
			}
		} catch (error) {
			if (error instanceof GenerationError) {
				throw error
			}

			throw this.errorHandler.createGenerationError('Failed to generate GraphQL schema', { originalError: error }, [
				'Check for errors in your ZModel schema',
				'Verify that all referenced models and fields exist',
				'Ensure proper configuration of plugin options',
			])
		}
	}

	private createScalarGenerator(): ScalarGenerator {
		return this.generatorFactory.createScalarGenerator()
	}

	private createEnumGenerator(): EnumGenerator {
		return this.generatorFactory.createEnumGenerator()
	}

	private createObjectTypeGenerator(): ObjectTypeGenerator {
		return this.generatorFactory.createObjectTypeGenerator()
	}

	private createRelationGenerator(): RelationGenerator {
		return this.generatorFactory.createRelationGenerator()
	}

	private createConnectionGenerator(): ConnectionGenerator {
		return this.generatorFactory.createConnectionGenerator()
	}

	addWarning(message: string): void {
		this.warnings.push(message)
	}

	getWarnings(): string[] {
		return this.warnings
	}

	private validateTypesWithRegistry(): void {
		try {
			const objectTypes = this.typeRegistry.getObjectTypes()
			const referencedTypes = new Set<string>()

			objectTypes.forEach((typeName) => {
				const typeComposer = this.schemaComposer.getOTC(typeName)
				if (typeComposer) {
					const fieldNames = typeComposer.getFieldNames()
					fieldNames.forEach((fieldName) => {
						const fieldType = typeComposer.getFieldType(fieldName)
						const fieldTypeName = fieldType.toString().replace(/[\[\]!]/g, '')
						if (this.schemaComposer.has(fieldTypeName)) {
							referencedTypes.add(fieldTypeName)
						}
					})
				}
			})

			objectTypes.forEach((typeName) => {
				if (!referencedTypes.has(typeName) && !typeName.includes('Query') && !typeName.includes('Mutation')) {
					this.warnings.push(`Potentially orphaned type detected: ${typeName} is not referenced by any other type`)
				}
			})
		} catch (error) {
			this.warnings.push(`Type validation warning: ${error instanceof Error ? error.message : String(error)}`)
		}
	}

	getTypeInfo(typeName: string): TypeInfo | undefined {
		const composer = this.typeRegistry.getTypeComposer(typeName)
		if (!composer) return undefined

		const kind = match<string, TypeKind>(typeName)
			.when(
				(name) => this.typeRegistry.isTypeOfKind(name, TypeKind.OBJECT),
				() => TypeKind.OBJECT
			)
			.when(
				(name) => this.typeRegistry.isTypeOfKind(name, TypeKind.SCALAR),
				() => TypeKind.SCALAR
			)
			.when(
				(name) => this.typeRegistry.isTypeOfKind(name, TypeKind.ENUM),
				() => TypeKind.ENUM
			)
			.when(
				(name) => this.typeRegistry.isTypeOfKind(name, TypeKind.CONNECTION),
				() => TypeKind.CONNECTION
			)
			.when(
				(name) => this.typeRegistry.isTypeOfKind(name, TypeKind.EDGE),
				() => TypeKind.EDGE
			)
			.otherwise(() => TypeKind.UNKNOWN)

		return {
			name: typeName,
			kind,
			composer,
			isGenerated: true,
		}
	}
}
