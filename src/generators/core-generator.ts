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
	private warnings: string[] = []
	private generatorFactory: GeneratorFactory

	constructor(context: GeneratorFactoryContext) {
		this.generatorFactory = new GeneratorFactory(context)
	}

	generate(): GenerationResult {
		this.generatorFactory.context.registry.addRelayRequirements()

		const scalarTypes = this.generatorFactory.create(ScalarGenerator).generate()
		const enumTypes = this.generatorFactory.create(EnumGenerator).generate()
		const objectTypes = this.generatorFactory.create(ObjectTypeGenerator).generate()
		const relationFields = this.generatorFactory.create(RelationGenerator).generate()

		let connectionTypes: string[] = []
		let sortInputTypes: string[] = []
		let filterInputTypes: string[] = []

		if (this.generatorFactory.context.options.connectionTypes) {
			connectionTypes = this.generatorFactory.create(ConnectionGenerator).generate()
			sortInputTypes = this.generatorFactory.create(SortInputGenerator).generate()
			filterInputTypes = this.generatorFactory.create(FilterInputGenerator).generate()
		}

		const validationErrors = this.generatorFactory.context.registry.validateSchema()
		if (validationErrors.length > 0) {
			this.warnings.push(...validationErrors)
		}

		const sdl = this.generatorFactory.context.registry.generateSDL()

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
