import { GeneratorContext } from '@core/types'
import { BaseGenerator } from '@generators/base-generator'
import { UnifiedGeneratorFactory, UnifiedGeneratorAdapter } from '@generators/unified'

export class GraphQLGeneratorFactory {
	constructor(private readonly context: GeneratorContext) {}

	createConnectionGenerator() {
		const unifiedGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(this.context, 'connectionGenerator')
		return new UnifiedGeneratorAdapter(this.context, unifiedGenerator)
	}

	createFilterInputGenerator() {
		const unifiedGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(this.context, 'filterInputGenerator')
		return new UnifiedGeneratorAdapter(this.context, unifiedGenerator)
	}

	createSortInputGenerator() {
		const unifiedGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(this.context, 'sortInputGenerator')
		return new UnifiedGeneratorAdapter(this.context, unifiedGenerator)
	}

	createObjectTypeGenerator() {
		const unifiedGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(this.context, 'objectTypeGenerator')
		return new UnifiedGeneratorAdapter(this.context, unifiedGenerator)
	}

	createRelationGenerator() {
		const unifiedGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(this.context, 'relationGenerator')
		return new UnifiedGeneratorAdapter(this.context, unifiedGenerator)
	}

	createGenerator<T extends BaseGenerator>(generatorType: new (context: GeneratorContext) => T): T {
		return new generatorType(this.context)
	}

	createAllGenerators() {
		return {
			connectionGenerator: this.createConnectionGenerator(),
			filterInputGenerator: this.createFilterInputGenerator(),
			sortInputGenerator: this.createSortInputGenerator(),
			objectTypeGenerator: this.createObjectTypeGenerator(),
			relationGenerator: this.createRelationGenerator(),
		}
	}
}
