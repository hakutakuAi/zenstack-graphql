import { GeneratorFactoryContext } from '@types'
import { GeneratorFactory } from '@/generators/generator-factory'
import { ScalarGenerator } from '@generators/scalar-generator'
import { EnumGenerator } from '@generators/enum-generator'
import { ObjectTypeGenerator } from '@generators/object-type-generator'
import { RelationGenerator } from '@generators/relation-generator'
import { ConnectionGenerator } from '@generators/connection-generator'
import { SortInputGenerator } from '@/generators/sort-input-generator'
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
	private warnings: string[] = []
	private generatorFactory: GeneratorFactory

	constructor(context: GeneratorFactoryContext) {
		this.generatorFactory = new GeneratorFactory(context)
	}

	generate(): GenerationResult {
		this.generatorFactory.context.registry.addRelayRequirements()

		const stats: GenerationStats = {
			objectTypes: [],
			enumTypes: [],
			scalarTypes: [],
			relationFields: [],
			connectionTypes: [],
			sortInputTypes: [],
			filterInputTypes: [],
		}

		if (this.generatorFactory.context.options.generateScalars) {
			stats.scalarTypes = this.generatorFactory.create(ScalarGenerator).generate()
		}

		if (this.generatorFactory.context.options.generateEnums) {
			stats.enumTypes = this.generatorFactory.create(EnumGenerator).generate()
		}

		stats.objectTypes = this.generatorFactory.create(ObjectTypeGenerator).generate()

		if (this.generatorFactory.context.options.includeRelations) {
			stats.relationFields = this.generatorFactory.create(RelationGenerator).generate()
		}

		if (this.generatorFactory.context.options.connectionTypes) {
			stats.connectionTypes = this.generatorFactory.create(ConnectionGenerator).generate()

			if (this.generatorFactory.context.options.generateSorts) {
				stats.sortInputTypes = this.generatorFactory.create(SortInputGenerator).generate()
			}

			if (this.generatorFactory.context.options.generateFilters) {
				stats.filterInputTypes = this.generatorFactory.create(FilterInputGenerator).generate()
			}
		}

		this.warnings.push(...this.generatorFactory.context.registry.validateSchema())

		const sdl = this.generatorFactory.context.registry.generateSDL()

		return {
			sdl,
			stats,
		}
	}
}
