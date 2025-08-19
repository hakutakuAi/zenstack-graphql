import { SchemaComposer } from 'graphql-compose'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface GeneratorFactoryContext {
	options: NormalizedOptions
	models: DataModel[]
	enums: Enum[]
}

export interface GeneratorContext extends GeneratorFactoryContext {
	schemaComposer: SchemaComposer<unknown>
	registry: GraphQLRegistry
	attributeProcessor: SchemaProcessor
	typeFormatter: TypeFormatter
	typeMapper: UnifiedTypeMapper
	typeFactories: GraphQLTypeFactories
}
