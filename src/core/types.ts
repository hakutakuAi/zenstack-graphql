import { SchemaComposer } from 'graphql-compose'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config/options-validator'
import { SchemaProcessor, GraphQLTypeFactories, Registry, TypeFormatter, TypeMapper } from '@/utils'

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
