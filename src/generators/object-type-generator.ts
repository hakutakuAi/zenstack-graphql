import { ObjectTypeComposer } from 'graphql-compose'
import { Result, ok, err } from 'neverthrow'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { ErrorCategory, logWarning } from '@utils/error'

export interface FieldConfig {
	type: string
	resolve?: any
}

export class ObjectTypeGenerator extends BaseGenerator {
	generate(): string[] {
		this.models.filter((model) => this.shouldGenerateModel(model)).forEach((model) => this.generateObjectType(model))
		return this.registry.getObjectTypes()
	}

	private generateObjectType(model: DataModel): void {
		const typeName = this.getObjectTypeName(model)

		if (this.hasObjectType(typeName)) {
			return
		}

		const fieldsResult = this.createObjectFields(model)
		if (fieldsResult.isErr()) {
			logWarning(`Failed to create fields for model ${model.name}: ${fieldsResult.error}`, ErrorCategory.GENERATION, {
				modelName: model.name,
				error: fieldsResult.error,
			})
			return
		}

		const description = this.getObjectTypeDescription(model)

		const objectComposer = this.schemaComposer.createObjectTC({
			name: typeName,
			description,
			fields: fieldsResult.value,
		})

		this.registry.registerType(typeName, TypeKind.OBJECT, objectComposer, true)
	}

	private getObjectTypeDescription(model: DataModel): string | undefined {
		return ValidationUtils.getModelDescription(model, this.attributeProcessor)
	}

	private createObjectFields(model: DataModel): Result<Record<string, FieldConfig>, string> {
		const fields: Record<string, FieldConfig> = {}

		for (const field of model.fields) {
			if (this.shouldIncludeField(model, field)) {
				const fieldName = this.getFieldName(model, field)
				const fieldConfigResult = this.createFieldConfig(field)

				if (fieldConfigResult.isErr()) {
					return err(fieldConfigResult.error)
				}

				fields[fieldName] = fieldConfigResult.value
			}
		}

		return ok(fields)
	}

	protected override shouldIncludeField(model: DataModel, field: DataModelField, includeRelations: boolean = true): boolean {
		return ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, this.options.includeRelations)
	}

	private getFieldName(model: DataModel, field: DataModelField): string {
		return this.getFormattedFieldName(model, field)
	}

	private createFieldConfig(field: DataModelField): Result<FieldConfig, string> {
		const graphqlTypeResult = this.mapFieldType(field)

		if (graphqlTypeResult.isErr()) {
			return err(graphqlTypeResult.error)
		}

		return ok({
			type: graphqlTypeResult.value,
		})
	}

	private mapFieldType(field: DataModelField): Result<string, string> {
		if (!this.typeMapper) {
			return err('TypeMapper is not initialized')
		}

		if (this.typeMapper.isRelationField(field)) {
			return ok(this.typeMapper.getRelationFieldType(field))
		}

		const mappedType = this.typeMapper.mapFieldType(field)
		if (!mappedType) {
			return err(`Unsupported field type: ${field.type}`)
		}

		return ok(mappedType)
	}

	hasObjectType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.OBJECT)
	}
}
