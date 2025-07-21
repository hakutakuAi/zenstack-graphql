import { ObjectTypeComposer } from 'graphql-compose'
import { Result, ok, err } from 'neverthrow'
import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { ErrorCategory, logWarning } from '@utils/error'

export interface FieldConfig {
	type: string
	resolve?: any
}

export class ObjectTypeGenerator extends BaseGenerator {
	generate(): string[] {
		this.models.filter((model) => !this.attributeProcessor.model(model).isIgnored()).forEach((model) => this.generateObjectType(model))
		return this.registry.getObjectTypes()
	}

	hasObjectType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.OBJECT)
	}

	private generateObjectType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)

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

		const description = this.attributeProcessor.model(model).description()

		const objectComposer = this.schemaComposer.createObjectTC({
			name: typeName,
			description,
			fields: fieldsResult.value,
		})

		this.registry.registerType(typeName, TypeKind.OBJECT, objectComposer, true)
	}

	private createObjectFields(model: DataModel): Result<Record<string, FieldConfig>, string> {
		const fields: Record<string, FieldConfig> = {}

		for (const field of model.fields) {
			if (this.attributeProcessor.field(model, field.name).shouldInclude(this.options.includeRelations)) {
				const fieldName = this.attributeProcessor.field(model, field.name).getFormattedFieldName(this.typeFormatter)
				const fieldConfigResult = this.createFieldConfig(field)

				if (fieldConfigResult.isErr()) {
					return err(fieldConfigResult.error)
				}

				fields[fieldName] = fieldConfigResult.value
			}
		}

		return ok(fields)
	}

	private createFieldConfig(field: DataModelField): Result<FieldConfig, string> {
		const graphqlTypeResult = this.mapFieldType(field)

		if (graphqlTypeResult.isErr()) {
			return err(graphqlTypeResult.error)
		}

		const model = this.models.find((m) => m.fields.includes(field))
		if (!model) {
			return err(`Could not find model for field ${field.name}`)
		}

		const description = this.attributeProcessor.field(model, field.name).description()

		return ok({
			type: graphqlTypeResult.value,
			description,
		})
	}

	private mapFieldType(field: DataModelField): Result<string, string> {
		if (this.typeMapper.isRelationField(field)) {
			return ok(this.typeMapper.getRelationFieldType(field))
		}

		const mappedType = this.typeMapper.mapFieldType(field)
		if (!mappedType) {
			return err(`Unsupported field type: ${field.type}`)
		}

		return ok(mappedType)
	}
}
