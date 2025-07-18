import { DMMF } from '@zenstackhq/sdk/prisma'
import { asDataModel } from '@types'
import { AttributeProcessor } from './attribute-processor'
import { HandleErrors } from '@utils/error/error-decorators'

export class ValidationUtils {
	@HandleErrors()
	static shouldGenerateModel(model: DMMF.Model, processor: AttributeProcessor): boolean {
		const dataModel = asDataModel(model)
		return !processor.attr(dataModel, '@@graphql.ignore').exists()
	}

	@HandleErrors()
	static shouldIncludeField(model: DMMF.Model, field: DMMF.Field, processor: AttributeProcessor, includeRelations: boolean): boolean {
		if (field.relationName && !includeRelations) {
			return false
		}

		return !processor.hasFieldIgnoreAttr(asDataModel(model), field.name)
	}

	@HandleErrors()
	static getModelName(model: DMMF.Model, processor: AttributeProcessor): string | undefined {
		const dataModel = asDataModel(model)
		return processor.attr(dataModel, '@@graphql.name').getString('name') || model.name
	}

	@HandleErrors()
	static getFieldName(model: DMMF.Model, fieldName: string, processor: AttributeProcessor): string | undefined {
		return processor.processField(asDataModel(model), fieldName).name() || fieldName
	}

	@HandleErrors()
	static getModelDescription(model: DMMF.Model, processor: AttributeProcessor): string | undefined {
		const dataModel = asDataModel(model)
		return processor.attr(dataModel, '@@graphql.description').getString('description') || model.documentation
	}

	@HandleErrors()
	static isFieldNonSortable(model: DMMF.Model, fieldName: string, processor: AttributeProcessor): boolean {
		return processor.processField(asDataModel(model), fieldName).isNonSortable()
	}

	@HandleErrors()
	static isFieldNonFilterable(model: DMMF.Model, fieldName: string, processor: AttributeProcessor): boolean {
		return processor.processField(asDataModel(model), fieldName).isNonFilterable()
	}

	@HandleErrors()
	static getFieldInputTypeName(model: DMMF.Model, fieldName: string, processor: AttributeProcessor): string | undefined {
		return processor.processField(asDataModel(model), fieldName).inputTypeName()
	}

	static extractErrorMessage(error: unknown): string {
		if (error instanceof Error) {
			return error.message
		}
		return String(error)
	}
}
