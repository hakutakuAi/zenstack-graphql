import { SchemaComposer } from 'graphql-compose'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Registry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { TypeMapper } from '@utils/schema/type-mapper'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface GeneratorFactoryContext {
	options: NormalizedOptions
	models: DataModel[]
	enums: Enum[]
}

export interface GeneratorContext extends GeneratorFactoryContext {
	schemaComposer: SchemaComposer<unknown>
	registry: Registry
	attributeProcessor: SchemaProcessor
	typeFormatter: TypeFormatter
	typeMapper: TypeMapper
	typeFactories: GraphQLTypeFactories
}
