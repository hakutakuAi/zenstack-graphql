import { ObjectTypeComposer } from 'graphql-compose'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext, DMMF } from '@types'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { Generate, SchemaOp, Validate } from '@utils/error'

export interface FieldConfig {
	type: string
	description?: string
	resolve?: any
}

export class ObjectTypeGenerator extends BaseGenerator {
	private readonly dmmfModels: readonly DMMF.Model[]

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.dmmfModels) {
			throw new Error('DMMF models are required for ObjectTypeGenerator')
		}
		this.dmmfModels = context.dmmfModels
	}

	@Generate({
		suggestions: ['Check model definitions in your schema', 'Ensure field types are valid GraphQL types', 'Verify model attributes are properly configured'],
	})
	generate(): void {
		this.dmmfModels.filter((model) => ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)).forEach((model) => this.generateObjectType(model))
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

	@SchemaOp({
		suggestions: ['Check model definition structure and content', 'Ensure model name is a valid GraphQL identifier', 'Verify all field types are supported'],
	})
	private generateObjectType(dmmfModel: DMMF.Model): void {
		const typeName = this.getObjectTypeName(dmmfModel)

		if (this.hasObjectType(typeName)) {
			return
		}

		const fields = this.createObjectFields(dmmfModel)
		const description = this.getObjectTypeDescription(dmmfModel)

		const objectComposer = this.schemaComposer.createObjectTC({
			name: typeName,
			description,
			fields,
		})

		this.registry.registerType(typeName, TypeKind.OBJECT, objectComposer, true)
	}

	private getObjectTypeName(dmmfModel: DMMF.Model): string {
		const customName = ValidationUtils.getModelName(dmmfModel, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || dmmfModel.name)
	}

	private getObjectTypeDescription(dmmfModel: DMMF.Model): string | undefined {
		return ValidationUtils.getModelDescription(dmmfModel, this.attributeProcessor)
	}

	private createObjectFields(dmmfModel: DMMF.Model): Record<string, FieldConfig> {
		return dmmfModel.fields
			.filter((field) => this.shouldIncludeField(dmmfModel, field))
			.reduce((fields, field) => {
				const fieldName = this.getFieldName(dmmfModel, field)
				const fieldConfig = this.createFieldConfig(field)
				return { ...fields, [fieldName]: fieldConfig }
			}, {})
	}

	private shouldIncludeField(dmmfModel: DMMF.Model, field: DMMF.Field): boolean {
		return ValidationUtils.shouldIncludeField(dmmfModel, field, this.attributeProcessor, this.options.includeRelations)
	}

	private getFieldName(dmmfModel: DMMF.Model, field: DMMF.Field): string {
		const customName = ValidationUtils.getFieldName(dmmfModel, field.name, this.attributeProcessor)
		return this.typeFormatter.formatFieldName(customName || field.name)
	}

	private createFieldConfig(field: DMMF.Field): FieldConfig {
		const graphqlType = this.mapFieldType(field)
		const description = field.documentation

		return {
			type: graphqlType,
			...(description && { description }),
		}
	}

	@Validate({
		suggestions: ['Check if the field type is supported', 'Add custom scalar mapping in options', 'Consider using a different field type'],
	})
	private mapFieldType(field: DMMF.Field): string {
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
