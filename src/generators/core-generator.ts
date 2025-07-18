import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error/error-handler'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedRegistry, TypeKind } from '@utils/registry/unified-registry'
import { GeneratorContext, DMMF } from '@types'
import { NormalizedOptions } from '@utils/config/options-validator'
import { GeneratorFactory } from '@utils/generator-factory'
import { ScalarGenerator } from '@generators/scalar-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'
import { Generate, Validate } from '@utils/error'

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
	private typeFormatter: TypeFormatter
	private options: NormalizedOptions
	private dmmf: DMMF.Document
	private registry: UnifiedRegistry
	private warnings: string[] = []
	private generatorFactory: GeneratorFactory

	constructor(context: GeneratorContext) {
		if (!context.model || !context.dmmf) {
			throw new Error('Model and DMMF document are required for CoreGenerator')
		}

		this.options = context.options
		this.dmmf = context.dmmf
		this.errorHandler = context.errorHandler || ErrorHandler.getInstance()
		this.attributeProcessor = context.attributeProcessor || new AttributeProcessor()
		this.typeMapper = context.typeMapper || TypeMapper.createFromDMMF(context.dmmf)
		this.typeFormatter = context.typeFormatter || TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming)
		this.schemaComposer = context.schemaComposer || new SchemaComposer()
		this.registry = context.registry || new UnifiedRegistry(this.schemaComposer, this.errorHandler)

		this.generatorFactory = new GeneratorFactory({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			typeFormatter: this.typeFormatter,
			dmmfModels: this.dmmf.datamodel.models,
			dmmfEnums: this.dmmf.datamodel.enums,
			registry: this.registry,
		})
	}

	@Generate({
		suggestions: ['Check for errors in your ZModel schema', 'Verify that all referenced models and fields exist', 'Ensure proper configuration of plugin options'],
	})
	generateSchema(): GenerationResult {
		if (this.options.relayCompliant) {
			this.registry.addRelayRequirements()
		}

		const scalarGenerator = this.createScalarGenerator()
		scalarGenerator.generate()
		const scalarTypes = scalarGenerator.getGeneratedScalars()

		scalarTypes.forEach((typeName) => {
			const composer = this.schemaComposer.getSTC(typeName)
			if (composer) {
				this.registry.registerType(typeName, TypeKind.SCALAR, composer, true)
			}
		})

		const enumGenerator = this.createEnumGenerator()
		enumGenerator.generate()
		const enumTypes = enumGenerator.getGeneratedEnums()

		enumTypes.forEach((typeName) => {
			const composer = this.schemaComposer.getETC(typeName)
			if (composer) {
				this.registry.registerType(typeName, TypeKind.ENUM, composer, true)
			}
		})

		const objectGenerator = this.createObjectTypeGenerator()
		objectGenerator.generate()
		const objectTypes = objectGenerator.getGeneratedObjectTypes()

		objectTypes.forEach((typeName) => {
			const composer = this.schemaComposer.getOTC(typeName)
			if (composer) {
				this.registry.registerType(typeName, TypeKind.OBJECT, composer, true)
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
					this.registry.registerType(typeName, TypeKind.CONNECTION, composer, true)
				}
			})

			const edgeTypes = connectionGenerator.getGeneratedEdgeTypes()
			edgeTypes.forEach((typeName) => {
				const composer = this.schemaComposer.getOTC(typeName)
				if (composer) {
					this.registry.registerType(typeName, TypeKind.EDGE, composer, true)
				}
			})
		}

		const validationErrors = this.registry.validateSchema()
		if (validationErrors.length > 0) {
			this.warnings.push(...validationErrors)
		}

		this.validateTypesWithRegistry()

		const sdl = this.registry.generateSDL()

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

	@Validate()
	private validateTypesWithRegistry(): void {
		const objectTypes = this.registry.getObjectTypes()

		const referencedTypes = new Set(
			objectTypes.flatMap((typeName) => {
				const typeComposer = this.schemaComposer.getOTC(typeName)
				if (!typeComposer) return []

				return typeComposer
					.getFieldNames()
					.map((fieldName) => {
						const fieldType = typeComposer.getFieldType(fieldName)
						const fieldTypeName = fieldType.toString().replace(/[\[\]!]/g, '')
						return this.schemaComposer.has(fieldTypeName) ? fieldTypeName : null
					})
					.filter(Boolean) as string[]
			})
		)

		const orphanedTypes = objectTypes.filter((typeName) => !referencedTypes.has(typeName) && !typeName.includes('Query') && !typeName.includes('Mutation'))

		orphanedTypes.forEach((typeName) => {
			this.warnings.push(`Potentially orphaned type detected: ${typeName} is not referenced by any other type`)
		})
	}
}
