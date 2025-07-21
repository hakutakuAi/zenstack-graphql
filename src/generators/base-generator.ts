import { SchemaComposer } from 'graphql-compose'
import { NormalizedOptions } from '@utils/config/options-validator'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { Registry } from '@/utils/registry/registry'
import { GeneratorContext } from '@types'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { GraphQLTypeFactories } from '@/utils'

export abstract class BaseGenerator<T = void> {
	protected readonly options: NormalizedOptions
	protected readonly enums: Enum[]
	protected readonly models: DataModel[]
	protected readonly schemaComposer: SchemaComposer<unknown>
	protected readonly registry: Registry
	protected readonly attributeProcessor: SchemaProcessor
	protected readonly typeFormatter: TypeFormatter
	protected readonly typeMapper: TypeMapper
	protected readonly typeFactories: GraphQLTypeFactories

	constructor(context: GeneratorContext) {
		this.options = context.options
		this.models = context.models
		this.enums = context.enums
		this.schemaComposer = context.schemaComposer
		this.registry = context.registry
		this.attributeProcessor = context.attributeProcessor
		this.typeFormatter = context.typeFormatter
		this.typeMapper = context.typeMapper
		this.typeFactories = context.typeFactories
	}

	abstract generate(): T
}
