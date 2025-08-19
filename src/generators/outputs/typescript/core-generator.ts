import { GeneratorFactoryContext } from '@core/types'
import { DataModel } from '@zenstackhq/sdk/ast'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedScalarGenerator } from '@generators/outputs/unified-scalar-generator'
import { UnifiedEnumGenerator } from '@generators/outputs/unified-enum-generator'
import { OutputFormat } from '@utils/constants'
import { TypeScriptGeneratorFactory } from './typescript-generator-factory'
import { TypeScriptFilterInputGenerator } from './filter-input-generator'
import { TypeScriptSortInputGenerator } from './sort-input-generator'
import { TypeScriptConnectionGenerator } from './connection-generator'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { SchemaProcessor } from '@utils/schema/schema-processor'

export interface TypeScriptGenerationStats {
	objectTypes: string[]
	enumTypes: string[]
	scalarTypes: string[]
	filterInputTypes: string[]
	sortInputTypes: string[]
	connectionTypes: string[]
}

export interface TypeScriptGenerationResult {
	code: string
	stats: TypeScriptGenerationStats
}

export class CoreGenerator {
	private astFactory: TypeScriptASTFactory
	private context: GeneratorFactoryContext
	private typeFormatter: TypeFormatter
	private typeMapper: UnifiedTypeMapper
	private generatorFactory: TypeScriptGeneratorFactory

	constructor(context: GeneratorFactoryContext) {
		this.context = context
		this.typeFormatter = new TypeFormatter(context.options.typeNaming, context.options.fieldNaming)
		this.typeMapper = new UnifiedTypeMapper(this.typeFormatter, context.models, context.enums, context.options)
		const schemaProcessor = new SchemaProcessor()
		this.astFactory = new TypeScriptASTFactory(this.typeFormatter, this.typeMapper, schemaProcessor)
		this.generatorFactory = new TypeScriptGeneratorFactory(context, this.astFactory)
	}

	generate(): TypeScriptGenerationResult {
		const { options, models } = this.context

		const objectTypes: string[] = []

		const scalarGenerator = new UnifiedScalarGenerator(this.context, OutputFormat.TYPE_GRAPHQL)
		const scalarResult = options.generateScalars ? scalarGenerator.generate() : { typescriptTypes: [] }

		const enumGenerator = new UnifiedEnumGenerator(this.context, OutputFormat.TYPE_GRAPHQL, this.typeFormatter)
		const enumResult = options.generateEnums ? enumGenerator.generate() : { typescriptTypes: [] }

		let filterResult: string[] = []
		let sortResult: string[] = []
		let connectionResult: string[] = []

		// Generate object types first to avoid circular reference issues
		const validModels = models.filter((model: DataModel) => !model.isAbstract)
		for (const model of validModels) {
			try {
				this.astFactory.createObjectType(model)
				objectTypes.push(model.name)
			} catch (error) {
				console.warn(`Failed to generate TypeScript object type for ${model.name}:`, error)
			}
		}

		// Generate supporting types after object types are defined
		if (options.generateFilters) {
			const filterGenerator = this.generatorFactory.create(TypeScriptFilterInputGenerator)
			filterResult = filterGenerator.generate()
		}

		if (options.generateSorts) {
			const sortGenerator = this.generatorFactory.create(TypeScriptSortInputGenerator)
			sortResult = sortGenerator.generate()
		}

		if (options.connectionTypes) {
			const connectionGenerator = this.generatorFactory.create(TypeScriptConnectionGenerator)
			connectionResult = connectionGenerator.generate()
		}

		const stats: TypeScriptGenerationStats = {
			objectTypes,
			enumTypes: enumResult.typescriptTypes,
			scalarTypes: scalarResult.typescriptTypes,
			filterInputTypes: filterResult,
			sortInputTypes: sortResult,
			connectionTypes: connectionResult,
		}

		const code = this.astFactory.getGeneratedCode()

		return {
			code,
			stats,
		}
	}
}
