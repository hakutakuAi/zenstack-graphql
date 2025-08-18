import { GeneratorContext, GeneratorFactoryContext } from '@types'
import { BaseGenerator } from '@generators/base-generator'
import { SchemaComposer } from 'graphql-compose'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Registry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { TypeMapper } from '@utils/schema/type-mapper'

type GeneratorConstructor<T extends BaseGenerator> = new (context: GeneratorContext) => T

export class GeneratorFactory {
	readonly context: GeneratorContext

	constructor(context: GeneratorFactoryContext) {
		const schemaComposer = new SchemaComposer()
		const typeFormatter = TypeFormatter.fromOptions(context.options.typeNaming, context.options.fieldNaming)

		this.context = {
			...context,
			schemaComposer,
			attributeProcessor: new SchemaProcessor(),
			registry: new Registry(schemaComposer),
			typeFormatter,
			typeMapper: TypeMapper.createFromModelsAndEnums(context.models, context.enums, context.options),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	create<T extends BaseGenerator>(generatorClass: GeneratorConstructor<T>): T {
		return new generatorClass(this.context)
	}
}
