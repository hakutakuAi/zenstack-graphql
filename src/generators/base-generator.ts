import { SchemaComposer } from 'graphql-compose'
import { NormalizedOptions } from '@utils/config/options-validator'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { Registry } from '@/utils/registry/registry'
import { GeneratorContext } from '@types'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'
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

	protected skipGeneration(): boolean {
		return false
	}

	protected getObjectTypeName(model: DataModel): string {
		const customName = this.attributeProcessor.model(model).name()
		const nameToFormat = customName !== undefined ? customName : model.name
		return this.typeFormatter.formatTypeName(nameToFormat)
	}

	protected shouldIncludeField(model: DataModel, field: DataModelField, includeRelations: boolean = true): boolean {
		return this.attributeProcessor.field(model, field.name).shouldInclude(includeRelations)
	}

	protected isFilterableField(model: DataModel, field: DataModelField): boolean {
		return this.attributeProcessor.field(model, field.name).isFilterable() && this.attributeProcessor.field(model, field.name).shouldInclude(true)
	}

	protected isSortableField(model: DataModel, field: DataModelField): boolean {
		const isAttrSortable = this.attributeProcessor.field(model, field.name).isSortable()
		const isTypeSortable = this.attributeProcessor.field(model, field.name).isSortableType()
		const shouldInclude = this.attributeProcessor.field(model, field.name).shouldInclude(true)

		return isAttrSortable && isTypeSortable && shouldInclude
	}

	protected getFormattedFieldName(model: DataModel, field: DataModelField): string {
		const customName = this.attributeProcessor.field(model, field.name).name()
		const nameToFormat = customName !== undefined ? customName : field.name
		return this.typeFormatter.formatFieldName(nameToFormat)
	}

	protected shouldGenerateModel(model: DataModel): boolean {
		return !this.attributeProcessor.model(model).isIgnored()
	}
}
