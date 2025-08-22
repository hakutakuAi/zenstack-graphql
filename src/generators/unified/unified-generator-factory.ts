import { BaseGeneratorContext, GeneratorContext } from '@core/types'
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
	private static createCommonGenerators(unifiedContext: any, originalContext: any) {
		return {
			sortInputGenerator: new UnifiedSortInputGenerator(unifiedContext),
			filterInputGenerator: new UnifiedFilterInputGenerator(unifiedContext),
			connectionGenerator: new UnifiedConnectionGenerator(unifiedContext),
			objectTypeGenerator: new UnifiedObjectTypeGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(originalContext),
			scalarGenerator: new UnifiedScalarGenerator(originalContext),
		}
	}

	static createGraphQLGenerators(context: GeneratorContext) {
		const unifiedContext = UnifiedContextFactory.createGraphQLContext(context)

		return {
			...this.createCommonGenerators(unifiedContext, context),
			relationGenerator: new UnifiedRelationGenerator(unifiedContext),
		}
	}

	static createTypeScriptGenerators(context: BaseGeneratorContext) {
		const unifiedContext = UnifiedContextFactory.createTypeScriptContext(context)

		return {
			...this.createCommonGenerators(unifiedContext, context),
			relationGenerator: new UnifiedRelationGenerator(unifiedContext),
			inputGenerator: new UnifiedInputGenerator(unifiedContext),
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
		context: BaseGeneratorContext,
		generatorType: T,
	): ReturnType<typeof UnifiedGeneratorFactory.createTypeScriptGenerators>[T] {
		const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(context)
		return generators[generatorType]
	}
}
