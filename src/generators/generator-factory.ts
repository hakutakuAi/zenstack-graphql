import { GeneratorContext, GeneratorFactoryContext } from '@core/types'
import { BaseGenerator } from '@generators/base-generator'
import { SchemaComposer } from 'graphql-compose'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'

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
			registry: new GraphQLRegistry(schemaComposer),
			typeFormatter,
			typeMapper: new UnifiedTypeMapper(typeFormatter, context.models, context.enums, context.options),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	create<T extends BaseGenerator>(generatorClass: GeneratorConstructor<T>): T {
		return new generatorClass(this.context)
	}
}
