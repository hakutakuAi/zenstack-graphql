import { GeneratorFactoryContext } from '@core/types'
import { GeneratorFactory } from '@generators/generator-factory'
import { UnifiedGeneratorFactory } from '@generators/unified'

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

		const generators = UnifiedGeneratorFactory.createGraphQLGenerators(this.generatorFactory.context)

		const scalarResult = options.generateScalars ? generators.scalarGenerator.generate() : { graphqlTypes: [] }
		const enumResult = options.generateEnums ? generators.enumGenerator.generate() : { graphqlTypes: [] }
		const objectTypes = generators.objectTypeGenerator.generate()
		const relationFields = options.includeRelations ? generators.relationGenerator.generate() : []
		const connectionTypes = options.connectionTypes ? generators.connectionGenerator.generate() : []
		const sortInputTypes = options.connectionTypes && options.generateSorts ? generators.sortInputGenerator.generate() : []
		const filterInputTypes = options.connectionTypes && options.generateFilters ? generators.filterInputGenerator.generate() : []

		const stats: GenerationStats = {
			objectTypes,
			enumTypes: enumResult.graphqlTypes,
			scalarTypes: scalarResult.graphqlTypes,
			relationFields,
			connectionTypes,
			sortInputTypes,
			filterInputTypes,
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
