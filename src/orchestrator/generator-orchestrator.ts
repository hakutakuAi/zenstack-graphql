import { BaseGeneratorContext, GeneratorContext, UnifiedGenerationResult, GenerationResult, GenerationType } from '@core/types'
import { OutputFormat } from '@utils/constants'
import { StatsCollector } from './stats-collector'
import {
	UnifiedGeneratorFactory,
	UnifiedContextFactory,
	UnifiedSortInputGenerator,
	UnifiedFilterInputGenerator,
	UnifiedConnectionGenerator,
	UnifiedObjectTypeGenerator,
	UnifiedInputGenerator,
	UnifiedQueryArgsGenerator,
	UnifiedRelationGenerator,
	UnifiedEnumGenerator,
	UnifiedScalarGenerator,
} from '@generators/unified'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { SchemaComposer } from 'graphql-compose'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'

interface TypeScriptGenerators {
	sortInputGenerator: UnifiedSortInputGenerator
	filterInputGenerator: UnifiedFilterInputGenerator
	connectionGenerator: UnifiedConnectionGenerator
	objectTypeGenerator: UnifiedObjectTypeGenerator
	enumGenerator: UnifiedEnumGenerator
	scalarGenerator: UnifiedScalarGenerator
	relationGenerator: UnifiedRelationGenerator
	inputGenerator: UnifiedInputGenerator
	queryArgsGenerator?: UnifiedQueryArgsGenerator
}

interface GraphQLGenerators {
	sortInputGenerator?: UnifiedSortInputGenerator
	filterInputGenerator?: UnifiedFilterInputGenerator
	connectionGenerator?: UnifiedConnectionGenerator
	objectTypeGenerator?: UnifiedObjectTypeGenerator
	enumGenerator?: UnifiedEnumGenerator
	scalarGenerator?: UnifiedScalarGenerator
	relationGenerator?: UnifiedRelationGenerator
	inputGenerator?: UnifiedInputGenerator
	queryArgsGenerator?: UnifiedQueryArgsGenerator
}

export class GeneratorOrchestrator {
	constructor(
		private readonly context: BaseGeneratorContext,
		private readonly outputFormat: OutputFormat,
	) {}

	async generate(): Promise<UnifiedGenerationResult> {
		if (this.outputFormat === OutputFormat.TYPE_GRAPHQL) {
			return this.generateTypeGraphQL()
		} else {
			return this.generateGraphQL()
		}
	}

	private async generateTypeGraphQL(): Promise<UnifiedGenerationResult> {
		const unifiedContext = UnifiedContextFactory.createTypeScriptContext(this.context)
		const generators = this.createTypeScriptGeneratorsWithContext(unifiedContext)
		const results = await this.executeGenerators(generators)

		return {
			code: unifiedContext.outputStrategy.getGeneratedCode?.() || '',
			results,
			stats: StatsCollector.collect(results),
			outputFormat: this.outputFormat,
		}
	}

	private createTypeScriptGeneratorsWithContext(unifiedContext: UnifiedGeneratorContext): TypeScriptGenerators {
		return {
			sortInputGenerator: new UnifiedSortInputGenerator(unifiedContext),
			filterInputGenerator: new UnifiedFilterInputGenerator(unifiedContext),
			connectionGenerator: new UnifiedConnectionGenerator(unifiedContext),
			objectTypeGenerator: new UnifiedObjectTypeGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(unifiedContext, OutputFormat.TYPE_GRAPHQL),
			scalarGenerator: new UnifiedScalarGenerator(this.context, OutputFormat.TYPE_GRAPHQL),
			relationGenerator: new UnifiedRelationGenerator(unifiedContext),
			inputGenerator: new UnifiedInputGenerator(unifiedContext),
		}
	}

	private async generateGraphQL(): Promise<UnifiedGenerationResult> {
		const graphqlContext = this.createGraphQLContext()
		const unifiedContext = UnifiedContextFactory.createGraphQLContext(graphqlContext)

		// Setup relay requirements for GraphQL
		graphqlContext.registry.addRelayRequirements()

		const generators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)
		const results = await this.executeGenerators(generators)

		// Validate schema and generate SDL
		const warnings = graphqlContext.registry.validateSchema()
		if (warnings.length > 0) {
			console.warn('Schema validation warnings:', warnings)
		}

		return {
			sdl: graphqlContext.registry.generateSDL(),
			results,
			stats: StatsCollector.collect(results),
			outputFormat: this.outputFormat,
		}
	}

	private createGraphQLContext(): GeneratorContext {
		const schemaComposer = new SchemaComposer()
		const typeFormatter = TypeFormatter.fromOptions(this.context.options.typeNaming, this.context.options.fieldNaming)

		return {
			...this.context,
			schemaComposer,
			attributeProcessor: new SchemaProcessor(),
			registry: new GraphQLRegistry(schemaComposer),
			typeFormatter,
			typeMapper: new UnifiedTypeMapper(typeFormatter, this.context.models, this.context.enums, this.context.options),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	private async executeGenerators(generators: TypeScriptGenerators | GraphQLGenerators): Promise<GenerationResult[]> {
		const results: GenerationResult[] = []
		const isTypeScript = this.outputFormat === OutputFormat.TYPE_GRAPHQL

		// Execute in proper order: scalars -> enums -> objects -> relations -> connections/sorts/filters

		if (this.context.options.generateScalars && generators.scalarGenerator) {
			const scalarResult = generators.scalarGenerator.generate()
			const items = isTypeScript ? scalarResult.typescriptTypes || [] : scalarResult.graphqlTypes || []
			results.push({
				items,
				count: items.length,
				type: GenerationType.SCALAR,
			})
		}

		if (this.context.options.generateEnums && generators.enumGenerator) {
			const enumResult = generators.enumGenerator.generate()
			results.push({
				items: enumResult,
				count: enumResult.length,
				type: GenerationType.ENUM,
			})
		}

		// Object types must be generated before relations since relations reference objects
		if (generators.objectTypeGenerator) {
			const objectResult = generators.objectTypeGenerator.generate()
			results.push({
				items: objectResult,
				count: objectResult.length,
				type: GenerationType.OBJECT,
			})
		}

		// Relations must come after objects since they reference object types
		if (this.context.options.includeRelations && generators.relationGenerator) {
			const relationResult = generators.relationGenerator.generate()
			results.push({
				items: relationResult,
				count: relationResult.length,
				type: GenerationType.RELATION,
			})
		}

		// Connections, sorts, and filters can come after objects and relations
		if (this.context.options.connectionTypes && generators.connectionGenerator) {
			const connectionResult = generators.connectionGenerator.generate()
			results.push({
				items: connectionResult,
				count: connectionResult.length,
				type: GenerationType.CONNECTION,
			})
		}

		if (this.context.options.generateSorts && generators.sortInputGenerator) {
			const sortResult = generators.sortInputGenerator.generate()
			results.push({
				items: sortResult,
				count: sortResult.length,
				type: GenerationType.SORT,
			})
		}

		if (this.context.options.generateFilters && generators.filterInputGenerator) {
			const filterResult = generators.filterInputGenerator.generate()
			results.push({
				items: filterResult,
				count: filterResult.length,
				type: GenerationType.FILTER,
			})
		}

		// Query args should come after filters and sorts since they reference them
		if (generators.queryArgsGenerator) {
			const queryArgsResult = generators.queryArgsGenerator.generate()
			results.push({
				items: queryArgsResult,
				count: queryArgsResult.length,
				type: GenerationType.INPUT,
			})
		}

		return results
	}
}
