import { BaseGeneratorContext, GeneratorContext } from '@core/types'
import { OutputFormat } from '@utils/constants'
import {
	UnifiedSortInputGenerator,
	UnifiedFilterInputGenerator,
	UnifiedConnectionGenerator,
	UnifiedObjectTypeGenerator,
	UnifiedInputGenerator,
	UnifiedQueryArgsGenerator,
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
			queryArgsGenerator: new UnifiedQueryArgsGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(unifiedContext, OutputFormat.GRAPHQL),
			scalarGenerator: new UnifiedScalarGenerator(context),
			relationGenerator: new UnifiedRelationGenerator(unifiedContext),
		}
	}

	static createTypeScriptGenerators(context: BaseGeneratorContext) {
		const unifiedContext = UnifiedContextFactory.createTypeScriptContext(context)

		return {
			sortInputGenerator: new UnifiedSortInputGenerator(unifiedContext),
			filterInputGenerator: new UnifiedFilterInputGenerator(unifiedContext),
			connectionGenerator: new UnifiedConnectionGenerator(unifiedContext),
			objectTypeGenerator: new UnifiedObjectTypeGenerator(unifiedContext),
			queryArgsGenerator: new UnifiedQueryArgsGenerator(unifiedContext),
			enumGenerator: new UnifiedEnumGenerator(unifiedContext, OutputFormat.TYPE_GRAPHQL),
			scalarGenerator: new UnifiedScalarGenerator(context),
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
