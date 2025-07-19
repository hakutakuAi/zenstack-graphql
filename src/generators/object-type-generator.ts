import { ObjectTypeComposer } from 'graphql-compose'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext } from '@types'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'

export interface FieldConfig {
	type: string
	resolve?: any
}

export class ObjectTypeGenerator extends BaseGenerator {
	private models: DataModel[]

	constructor(context: GeneratorContext) {
		super(context)
		this.models = context.models
	}

	generate(): void {
		this.models.filter((model) => this.shouldGenerateModel(model)).forEach((model) => this.generateObjectType(model))
	}

	getGeneratedObjectTypes(): string[] {
		return this.registry.getObjectTypes()
	}

	hasObjectType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.OBJECT)
	}

	getObjectComposer(name: string): ObjectTypeComposer | undefined {
		return this.registry.getObjectComposer(name)
	}

	getObjectTypeFields(typeName: string): string[] {
		return this.registry.getObjectTypeFields(typeName)
	}

	hasField(typeName: string, fieldName: string): boolean {
		return this.registry.hasField(typeName, fieldName)
	}

	getFieldType(typeName: string, fieldName: string): string | undefined {
		return this.registry.getFieldType(typeName, fieldName)
	}

	private generateObjectType(model: DataModel): void {
		const typeName = this.getObjectTypeName(model)

		if (this.hasObjectType(typeName)) {
			return
		}

		const fields = this.createObjectFields(model)
		const description = this.getObjectTypeDescription(model)

		const objectComposer = this.schemaComposer.createObjectTC({
			name: typeName,
			description,
			fields,
		})

		this.registry.registerType(typeName, TypeKind.OBJECT, objectComposer, true)
	}

	private getObjectTypeDescription(model: DataModel): string | undefined {
		return ValidationUtils.getModelDescription(model, this.attributeProcessor)
	}

	private createObjectFields(model: DataModel): Record<string, FieldConfig> {
		return model.fields
			.filter((field) => this.shouldIncludeField(model, field))
			.reduce((fields, field) => {
				const fieldName = this.getFieldName(model, field)
				const fieldConfig = this.createFieldConfig(field)
				return { ...fields, [fieldName]: fieldConfig }
			}, {})
	}

	protected override shouldIncludeField(model: DataModel, field: DataModelField, includeRelations: boolean = true): boolean {
		return ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, this.options.includeRelations)
	}

	private getFieldName(model: DataModel, field: DataModelField): string {
		return this.getFormattedFieldName(model, field)
	}

	private createFieldConfig(field: DataModelField): FieldConfig {
		const graphqlType = this.mapFieldType(field)

		return {
			type: graphqlType,
		}
	}

	private mapFieldType(field: DataModelField): string {
		if (!this.typeMapper) {
			throw new Error('TypeMapper is not initialized')
		}

		if (this.typeMapper.isRelationField(field)) {
			return this.typeMapper.getRelationFieldType(field)
		}

		const mappedType = this.typeMapper.mapFieldType(field)
		if (!mappedType) {
			throw new Error(`Unsupported field type: ${field.type}`)
		}

		return mappedType
	}
}
