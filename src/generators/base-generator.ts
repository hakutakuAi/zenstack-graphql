import { SchemaComposer } from 'graphql-compose'
import { NormalizedOptions } from '@utils/config/options-validator'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { Registry } from '@/utils/registry/registry'
import { GeneratorContext } from '@types'
import { ValidationUtils } from '@utils/schema/validation'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'
import { GraphQLTypeFactories } from '@/utils'

export abstract class BaseGenerator<T = void> {
	protected readonly options: NormalizedOptions
	protected readonly enums: Enum[]
	protected readonly models: DataModel[]
	protected readonly schemaComposer: SchemaComposer<unknown>
	protected readonly registry: Registry
	protected readonly attributeProcessor: AttributeProcessor
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
		const customName = ValidationUtils.getModelName(model, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || model.name)
	}

	protected shouldIncludeField(model: DataModel, field: DataModelField, includeRelations: boolean = true): boolean {
		return ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, includeRelations)
	}

	protected isFilterableField(model: DataModel, field: DataModelField): boolean {
		return ValidationUtils.isFieldFilterable(model, field.name, this.attributeProcessor) && this.shouldIncludeField(model, field, true)
	}

	protected isSortableField(model: DataModel, field: DataModelField): boolean {
		const isAttrSortable = ValidationUtils.isFieldSortable(model, field.name, this.attributeProcessor)
		const isTypeSortable = ValidationUtils.isSortableFieldType(field)
		const shouldInclude = this.shouldIncludeField(model, field, true)

		return isAttrSortable && isTypeSortable && shouldInclude
	}

	protected getFormattedFieldName(model: DataModel, field: DataModelField): string {
		const customName = ValidationUtils.getFieldName(model, field.name, this.attributeProcessor)
		return this.typeFormatter.formatFieldName(customName || field.name)
	}

	protected shouldGenerateModel(model: DataModel): boolean {
		return ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)
	}
}
