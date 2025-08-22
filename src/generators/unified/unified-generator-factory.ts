import { GeneratorFactoryContext, GeneratorContext } from '@core/types'
import {
	UnifiedSortInputGenerator,
	UnifiedFilterInputGenerator,
	UnifiedConnectionGenerator,
	UnifiedObjectTypeGenerator,
	UnifiedInputGenerator,
	UnifiedRelationGenerator,
	UnifiedEnumGenerator,
	UnifiedScalarGenerator,
	UnifiedContextFactory,
} from '@generators/unified'

export class UnifiedGeneratorFactory {
	static createGraphQLGenerators(context: GeneratorContext) {
		const unifiedContext = UnifiedContextFactory.createGraphQLContext(context)

		return {
			sortInputGenerator: new UnifiedSortInputGenerator(unifiedContext),
			filterInputGenerator: new UnifiedFilterInputGenerator(unifiedContext),
			connectionGenerator: new UnifiedConnectionGenerator(unifiedContext),
			objectTypeGenerator: new UnifiedObjectTypeGenerator(unifiedContext),
			relationGenerator: new UnifiedRelationGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(context),
			scalarGenerator: new UnifiedScalarGenerator(context),
		}
	}

	static createTypeScriptGenerators(context: GeneratorFactoryContext) {
		const unifiedContext = UnifiedContextFactory.createTypeScriptContext(context)

		return {
			sortInputGenerator: new UnifiedSortInputGenerator(unifiedContext),
			filterInputGenerator: new UnifiedFilterInputGenerator(unifiedContext),
			connectionGenerator: new UnifiedConnectionGenerator(unifiedContext),
			objectTypeGenerator: new UnifiedObjectTypeGenerator(unifiedContext),
			inputGenerator: new UnifiedInputGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(context),
			scalarGenerator: new UnifiedScalarGenerator(context),
		}
	}

	static createGraphQLGenerator<T extends keyof ReturnType<typeof UnifiedGeneratorFactory.createGraphQLGenerators>>(
		context: GeneratorContext,
		generatorType: T,
	): ReturnType<typeof UnifiedGeneratorFactory.createGraphQLGenerators>[T] {
		const generators = UnifiedGeneratorFactory.createGraphQLGenerators(context)
		return generators[generatorType]
	}

	static createTypeScriptGenerator<T extends keyof ReturnType<typeof UnifiedGeneratorFactory.createTypeScriptGenerators>>(
		context: GeneratorFactoryContext,
		generatorType: T,
	): ReturnType<typeof UnifiedGeneratorFactory.createTypeScriptGenerators>[T] {
		const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(context)
		return generators[generatorType]
	}
}
