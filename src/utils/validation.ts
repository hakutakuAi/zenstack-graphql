import { DMMF } from '@zenstackhq/sdk/prisma'
import { asDataModel } from '@types'

export class ValidationUtils {
	static shouldGenerateModel(model: DMMF.Model, attributeProcessor: any): boolean {
		try {
			return !attributeProcessor.hasModelIgnoreAttr(asDataModel(model))
		} catch (error) {
			return true
		}
	}

	static shouldIncludeField(model: DMMF.Model, field: DMMF.Field, attributeProcessor: any, includeRelations: boolean): boolean {
		try {
			if (attributeProcessor.hasFieldIgnoreAttr(asDataModel(model), field.name)) {
				return false
			}

			if (field.relationName && !includeRelations) {
				return false
			}

			return true
		} catch (error) {
			return true
		}
	}

	static getModelName(model: DMMF.Model, attributeProcessor: any): string | undefined {
		try {
			return attributeProcessor.getModelName(asDataModel(model))
		} catch (error) {
			return undefined
		}
	}

	static getFieldName(model: DMMF.Model, fieldName: string, attributeProcessor: any): string | undefined {
		try {
			return attributeProcessor.getFieldName(asDataModel(model), fieldName)
		} catch (error) {
			return undefined
		}
	}

	static getModelDescription(model: DMMF.Model, attributeProcessor: any): string | undefined {
		try {
			return attributeProcessor.getModelDescription(asDataModel(model)) || model.documentation
		} catch (error) {
			return model.documentation
		}
	}

	static extractErrorMessage(error: unknown): string {
		if (error instanceof Error) {
			return error.message
		}
		return String(error)
	}
}
