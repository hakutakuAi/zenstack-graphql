import { SchemaComposer } from 'graphql-compose'
import { Model, DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config/options-validator'
import { ErrorHandler } from '@utils/error/error-handler'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedRegistry } from '@utils/registry/unified-registry'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface GeneratorContext {
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	typeMapper: TypeMapper
	typeFormatter: TypeFormatter
	schemaComposer: SchemaComposer<unknown>
	registry: UnifiedRegistry
	models: DataModel[]
	enums: Enum[]
}
