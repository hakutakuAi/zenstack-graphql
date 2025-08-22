import { GeneratorFactoryContext, GeneratorContext } from '@core/types'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { GraphQLOutputStrategy } from '@generators/strategies/graphql-output-strategy'
import { TypeScriptOutputStrategy } from '@generators/strategies/typescript-output-strategy'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'

export class UnifiedContextFactory {
	static createGraphQLContext(graphqlContext: GeneratorContext): UnifiedGeneratorContext {
		const outputStrategy = new GraphQLOutputStrategy(
			graphqlContext.registry,
			graphqlContext.schemaComposer,
			graphqlContext.typeFactories,
			graphqlContext.options,
		)

		return {
			options: graphqlContext.options,
			models: graphqlContext.models,
			enums: graphqlContext.enums,
			typeFormatter: graphqlContext.typeFormatter,
			attributeProcessor: graphqlContext.attributeProcessor,
			typeMapper: graphqlContext.typeMapper,
			outputStrategy,
		}
	}

	static createTypeScriptContext(factoryContext: GeneratorFactoryContext): UnifiedGeneratorContext {
		const typeFormatter = new TypeFormatter(factoryContext.options.typeNaming, factoryContext.options.fieldNaming)
		const astFactory = new TypeScriptASTFactory(typeFormatter)
		const outputStrategy = new TypeScriptOutputStrategy(astFactory)
		const attributeProcessor = new SchemaProcessor()
		const typeMapper = new UnifiedTypeMapper(typeFormatter, factoryContext.models, factoryContext.enums, factoryContext.options)

		return {
			options: factoryContext.options,
			models: factoryContext.models,
			enums: factoryContext.enums,
			typeFormatter,
			attributeProcessor,
			typeMapper,
			outputStrategy,
		}
	}
}
