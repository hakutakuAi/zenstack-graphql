import { GeneratorFactoryContext } from '@core/types'
import { BaseGenerator } from '@generators/base-generator'
import { SchemaComposer } from 'graphql-compose'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'

export interface TypeScriptGeneratorContext {
	options: GeneratorFactoryContext['options']
	models: GeneratorFactoryContext['models']
	enums: GeneratorFactoryContext['enums']
	astFactory: TypeScriptASTFactory
	typeFormatter: TypeFormatter
	schemaComposer: SchemaComposer<unknown>
	registry: GraphQLRegistry
	attributeProcessor: SchemaProcessor
	typeMapper: UnifiedTypeMapper
	typeFactories: GraphQLTypeFactories
}

type TypeScriptGeneratorConstructor<T extends BaseGenerator> = new (context: TypeScriptGeneratorContext) => T

export class TypeScriptGeneratorFactory {
	readonly context: TypeScriptGeneratorContext

	constructor(context: GeneratorFactoryContext, astFactory: TypeScriptASTFactory) {
		const schemaComposer = new SchemaComposer()
		const typeFormatter = new TypeFormatter(context.options.typeNaming, context.options.fieldNaming)

		this.context = {
			...context,
			astFactory,
			typeFormatter,
			schemaComposer,
			attributeProcessor: new SchemaProcessor(),
			registry: new GraphQLRegistry(schemaComposer),
			typeMapper: new UnifiedTypeMapper(typeFormatter, context.models, context.enums, context.options),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	create<T extends BaseGenerator>(generatorClass: TypeScriptGeneratorConstructor<T>): T {
		return new generatorClass(this.context)
	}
}