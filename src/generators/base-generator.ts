import { SchemaComposer } from 'graphql-compose'
import { NormalizedOptions } from '@utils/config'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { GraphQLRegistry } from '@utils/registry'
import { GeneratorContext } from '@core/types'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'

export abstract class BaseGenerator<T = void> {
	protected readonly options: NormalizedOptions
	protected readonly enums: Enum[]
	protected readonly models: DataModel[]
	protected readonly schemaComposer: SchemaComposer<unknown>
	protected readonly registry: GraphQLRegistry
	protected readonly attributeProcessor: SchemaProcessor
	protected readonly typeFormatter: TypeFormatter
	protected readonly typeMapper: UnifiedTypeMapper
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

	protected get validModels() {
		return this.models.filter((model) => !this.attributeProcessor.model(model).isIgnored())
	}

	protected forEachValidModel(callback: (model: DataModel) => void): void {
		this.validModels.forEach(callback)
	}

	abstract generate(): T
}
