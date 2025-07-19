import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { AttributeProcessor } from './attribute-processor'

export class ValidationUtils {
	static shouldGenerateModel(model: DataModel, processor: AttributeProcessor): boolean {
		return !processor.attr(model, '@@graphql.ignore').exists()
	}

	static shouldIncludeField(model: DataModel, field: DataModelField, processor: AttributeProcessor, includeRelations: boolean): boolean {
		const isRelation = field.type.reference && field.type.reference.ref?.name && !field.type.type

		if (isRelation && !includeRelations) {
			return false
		}

		return !processor.processField(model, field.name).isIgnored()
	}

	static getModelName(model: DataModel, processor: AttributeProcessor): string | undefined {
		return processor.processModel(model).getString('name') || model.name
	}

	static getFieldName(model: DataModel, fieldName: string, processor: AttributeProcessor): string | undefined {
		return processor.processField(model, fieldName).name() || fieldName
	}

	static getModelDescription(model: DataModel, processor: AttributeProcessor): string | undefined {
		return processor.processModel(model).getString('description')
	}

	static isFieldSortable(model: DataModel, fieldName: string, processor: AttributeProcessor): boolean {
		return processor.processField(model, fieldName).isSortable()
	}

	static isFieldFilterable(model: DataModel, fieldName: string, processor: AttributeProcessor): boolean {
		return processor.processField(model, fieldName).isFilterable()
	}

	static isSortableFieldType(field: DataModelField): boolean {
		const fieldType = field.type.type

		if (!fieldType) return false

		switch (fieldType) {
			case 'Int':
			case 'Float':
			case 'Decimal':
			case 'DateTime':
			case 'String':
			case 'Boolean':
				return true
			default:
				return false
		}
	}

	static isRangeFilterableType(field: DataModelField): boolean {
		const fieldType = field.type.type

		if (!fieldType) return false

		switch (fieldType) {
			case 'Int':
			case 'Float':
			case 'Decimal':
			case 'DateTime':
				return true
			default:
				return false
		}
	}

	static isStringSearchableType(field: DataModelField): boolean {
		return field.type.type === 'String'
	}

	static extractErrorMessage(error: unknown): string {
		if (error instanceof Error) {
			return error.message
		}
		return String(error)
	}
}
