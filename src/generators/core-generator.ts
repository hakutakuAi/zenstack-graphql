import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error/error-handler'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedRegistry, TypeKind } from '@utils/registry/unified-registry'
import { GeneratorContext } from '@types'
import { NormalizedOptions } from '@utils/config/options-validator'
import { GeneratorFactory } from '@utils/generator-factory'
import { ScalarGenerator } from '@generators/scalar-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'
import { SortInputGenerator } from '@generators/sort-input-generator'
import { FilterInputGenerator } from '@generators/filter-input-generator'
import { Generate, Validate } from '@utils/error'

export interface GenerationStats {
	objectTypes: number
	enumTypes: number
	scalarTypes: number
	relationFields: number
	connectionTypes: number
	sortInputTypes: number
	filterInputTypes: number
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
	private registry: UnifiedRegistry
	private warnings: string[] = []
	private generatorFactory: GeneratorFactory

	constructor(context: GeneratorContext) {
		this.options = context.options
		this.errorHandler = context.errorHandler
		this.attributeProcessor = context.attributeProcessor
		this.typeMapper = context.typeMapper
		this.typeFormatter = context.typeFormatter
		this.schemaComposer = context.schemaComposer
		this.registry = context.registry

		this.generatorFactory = new GeneratorFactory({
			schemaComposer: this.schemaComposer,
			options: this.options,
			errorHandler: this.errorHandler,
			attributeProcessor: this.attributeProcessor,
			typeMapper: this.typeMapper,
			typeFormatter: this.typeFormatter,
			models: context.models,
			enums: context.enums,
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
		let sortInputTypes: string[] = []
		let filterInputTypes: string[] = []

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

			const sortInputGenerator = this.createSortInputGenerator()
			sortInputGenerator.generate()
			sortInputTypes = sortInputGenerator.getGeneratedSortInputTypes()

			const filterInputGenerator = this.createFilterInputGenerator()
			filterInputGenerator.generate()
			filterInputTypes = filterInputGenerator.getGeneratedFilterInputTypes()
		}

		const validationErrors = this.registry.validateSchema()
		if (validationErrors.length > 0) {
			this.warnings.push(...validationErrors)
		}

		const sdl = this.registry.generateSDL()

		return {
			sdl,
			stats: {
				objectTypes: objectTypes.length,
				enumTypes: enumTypes.length,
				scalarTypes: scalarTypes.length,
				relationFields: relationFields.length,
				connectionTypes: connectionTypes.length,
				sortInputTypes: sortInputTypes.length,
				filterInputTypes: filterInputTypes.length,
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

	private createSortInputGenerator(): SortInputGenerator {
		return this.generatorFactory.createSortInputGenerator()
	}

	private createFilterInputGenerator(): FilterInputGenerator {
		return this.generatorFactory.createFilterInputGenerator()
	}

	addWarning(message: string): void {
		this.warnings.push(message)
	}

	getWarnings(): string[] {
		return this.warnings
	}
}
