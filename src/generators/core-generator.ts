import { GeneratorFactoryContext } from '@types'
import { GeneratorFactory } from '@generators/generator-factory'
import { ScalarGenerator } from '@generators/scalar-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'
import { SortInputGenerator } from '@generators/sort-input-generator'
import { FilterInputGenerator } from '@generators/filter-input-generator'

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

		const stats: GenerationStats = {
			objectTypes: this.generatorFactory.create(ObjectTypeGenerator).generate(),
			enumTypes: options.generateEnums ? this.generatorFactory.create(EnumGenerator).generate() : [],
			scalarTypes: options.generateScalars ? this.generatorFactory.create(ScalarGenerator).generate() : [],
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
