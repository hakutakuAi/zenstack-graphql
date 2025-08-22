import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError, warning } from '@utils/error'

export interface FieldConfig {
	type: string
	description?: string
	resolve?: any
}

export class UnifiedObjectTypeGenerator extends UnifiedGeneratorBase {
	constructor(context: UnifiedGeneratorContext) {
		super(context)
	}

	protected generateForModel(model: DataModel): string | null {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)

		if (this.outputStrategy.hasType(typeName)) {
			return null
		}

		try {
			const fields = this.createObjectFields(model)
			const description = this.attributeProcessor.model(model).description()

			return this.outputStrategy.createObjectType(typeName, fields, description)
		} catch (error) {
			if (error instanceof PluginError) {
				warning(`Failed to create object type for model ${model.name}: ${error.message}`, error.category, {
					modelName: model.name,
					error: error,
				})
			} else {
				warning(
					`Failed to create object type for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
					ErrorCategory.GENERATION,
					{
						modelName: model.name,
						error: error,
					},
				)
			}
			return null
		}
	}

	private createObjectFields(model: DataModel): Record<string, FieldConfig> {
		const fields: Record<string, FieldConfig> = {}

		for (const field of model.fields) {
			if (this.attributeProcessor.field(model, field.name).shouldInclude(this.options.includeRelations)) {
				const fieldName = this.attributeProcessor.field(model, field.name).getFormattedFieldName(this.typeFormatter)
				const fieldConfig = this.createFieldConfig(field)
				fields[fieldName] = fieldConfig
			}
		}

		return fields
	}

	private createFieldConfig(field: DataModelField): FieldConfig {
		const graphqlType = this.mapFieldType(field)

		const model = this.models.find((m) => m.fields.includes(field))
		if (!model) {
			throw new PluginError(`Could not find model for field ${field.name}`, ErrorCategory.SCHEMA, { fieldName: field.name })
		}

		const description = this.attributeProcessor.field(model, field.name).description()

		return {
			type: graphqlType,
			description,
		}
	}

	private mapFieldType(field: DataModelField): string {
		if (this.typeMapper && this.typeMapper.isRelationField(field)) {
			return this.typeMapper.getRelationFieldType(field)
		}

		const mappedType = this.typeMapper && this.typeMapper.mapFieldType(field)
		if (!mappedType) {
			throw new PluginError(`Unsupported field type: ${field.type}`, ErrorCategory.SCHEMA, { field: field.name, type: field.type })
		}

		return mappedType
	}
}
