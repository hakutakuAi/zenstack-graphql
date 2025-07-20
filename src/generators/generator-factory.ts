import { GeneratorContext, GeneratorFactoryContext } from '@types'
import { BaseGenerator } from '@generators/base-generator'
import { SchemaComposer } from 'graphql-compose'
import { SchemaProcessor, GraphQLTypeFactories, Registry, TypeFormatter, TypeMapper } from '@/utils'

type GeneratorConstructor<T extends BaseGenerator> = new (context: GeneratorContext) => T

export class GeneratorFactory {
	private readonly baseContext: GeneratorContext

	constructor(context: GeneratorFactoryContext) {
		const dummyContext: Partial<GeneratorContext> = {
			options: context.options,
			models: context.models,
			enums: context.enums,
		}

		dummyContext.schemaComposer = new SchemaComposer()
		dummyContext.attributeProcessor = new SchemaProcessor()
		dummyContext.registry = new Registry(dummyContext.schemaComposer)
		dummyContext.typeFormatter = TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming)
		dummyContext.typeMapper = TypeMapper.createFromModelsAndEnums(context.models, context.enums)
		dummyContext.typeFactories = new GraphQLTypeFactories(dummyContext.schemaComposer, dummyContext.typeFormatter)

		this.baseContext = dummyContext as GeneratorContext
	}

	get context(): GeneratorContext {
		return this.baseContext
	}

	create<T extends BaseGenerator>(generatorClass: GeneratorConstructor<T>): T {
		return new generatorClass(this.context)
	}
}
