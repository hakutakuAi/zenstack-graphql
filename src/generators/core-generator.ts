import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error/error-handler'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { Registry, TypeKind } from '@/utils/registry/registry'
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

export interface GenerationStats {
	objectTypes: number
	enumTypes: number
	scalarTypes: number
	relationFields: number
	connectionTypes: number
	sortInputTypes: number
	filterInputTypes: number
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
	private registry: Registry
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

	generateSchema(): GenerationResult {
		if (this.options.relayCompliant) {
			this.registry.addRelayRequirements()
		}

		const scalarGenerator = this.generatorFactory.create(ScalarGenerator)
		scalarGenerator.generate()
		const scalarTypes = scalarGenerator.getGeneratedScalars()

		const enumGenerator = this.generatorFactory.create(EnumGenerator)
		enumGenerator.generate()
		const enumTypes = enumGenerator.getGeneratedEnums()

		const objectGenerator = this.generatorFactory.create(ObjectTypeGenerator)
		objectGenerator.generate()
		const objectTypes = objectGenerator.getGeneratedObjectTypes()

		const relationGenerator = this.generatorFactory.create(RelationGenerator)
		relationGenerator.generate()
		const relationFields = relationGenerator.getGeneratedRelations()

		let connectionTypes: string[] = []
		let sortInputTypes: string[] = []
		let filterInputTypes: string[] = []

		if (this.options.connectionTypes) {
			const connectionGenerator = this.generatorFactory.create(ConnectionGenerator)
			connectionGenerator.generate()

			connectionTypes = connectionGenerator.getGeneratedConnectionTypes()

			const sortInputGenerator = this.generatorFactory.create(SortInputGenerator)
			sortInputGenerator.generate()
			sortInputTypes = sortInputGenerator.getGeneratedSortInputTypes()

			const filterInputGenerator = this.generatorFactory.create(FilterInputGenerator)
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
			},
		}
	}
}
