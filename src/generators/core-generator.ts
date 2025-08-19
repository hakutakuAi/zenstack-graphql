import { GeneratorFactoryContext } from '@core/types'
import { GeneratorFactory } from '@generators/generator-factory'
import { UnifiedScalarGenerator } from '@generators/outputs/unified-scalar-generator'
import { UnifiedEnumGenerator } from '@generators/outputs/unified-enum-generator'
import { OutputFormat } from '@utils/constants'
import { ObjectTypeGenerator } from '@generators/outputs/graphql/object-type-generator'
import { RelationGenerator } from '@generators/outputs/graphql/relation-generator'
import { ConnectionGenerator } from '@generators/outputs/graphql/connection-generator'
import { SortInputGenerator } from '@generators/outputs/graphql/sort-input-generator'
import { FilterInputGenerator } from '@generators/outputs/graphql/filter-input-generator'

export interface GenerationStats {
	objectTypes: string[]
	enumTypes: string[]
	scalarTypes: string[]
	relationFields: string[]
	connectionTypes: string[]
	sortInputTypes: string[]
	filterInputTypes: string[]
}

export interface GenerationResult {
	sdl: string
	stats: GenerationStats
}

export class CoreGenerator {
	private generatorFactory: GeneratorFactory

	constructor(context: GeneratorFactoryContext) {
		this.generatorFactory = new GeneratorFactory(context)
	}

	generate(): GenerationResult {
		const { options, registry } = this.generatorFactory.context
		registry.addRelayRequirements()

		const scalarGenerator = new UnifiedScalarGenerator(this.generatorFactory.context, OutputFormat.GRAPHQL)
		const scalarResult = options.generateScalars ? scalarGenerator.generate() : { graphqlTypes: [] }

		const enumGenerator = new UnifiedEnumGenerator(this.generatorFactory.context, OutputFormat.GRAPHQL)
		const enumResult = options.generateEnums ? enumGenerator.generate() : { graphqlTypes: [] }

		const stats: GenerationStats = {
			objectTypes: this.generatorFactory.create(ObjectTypeGenerator).generate(),
			enumTypes: enumResult.graphqlTypes,
			scalarTypes: scalarResult.graphqlTypes,
			relationFields: options.includeRelations ? this.generatorFactory.create(RelationGenerator).generate() : [],
			connectionTypes: options.connectionTypes ? this.generatorFactory.create(ConnectionGenerator).generate() : [],
			sortInputTypes: options.connectionTypes && options.generateSorts ? this.generatorFactory.create(SortInputGenerator).generate() : [],
			filterInputTypes: options.connectionTypes && options.generateFilters ? this.generatorFactory.create(FilterInputGenerator).generate() : [],
		}

		const warnings = registry.validateSchema()
		if (warnings.length > 0) {
			console.warn('Schema validation warnings:', warnings)
		}

		return {
			sdl: registry.generateSDL(),
			stats,
		}
	}
}
