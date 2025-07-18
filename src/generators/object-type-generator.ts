import { SchemaComposer, ObjectTypeComposer } from 'graphql-compose'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/validation'
import { AttributeProcessor } from '@utils/attribute-processor'
import { ErrorHandler } from '@utils/error-handler'
import { TypeMapper } from '@utils/type-mapper'
import { NormalizedOptions } from '@utils/options-validator'

export interface ObjectTypeGeneratorContext {
	schemaComposer: SchemaComposer
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	typeMapper: TypeMapper
	dmmfModels: readonly DMMF.Model[]
}

export interface FieldConfig {
	type: string
	description?: string
	resolve?: any
}

export class ObjectTypeGenerator extends BaseGenerator<ObjectTypeComposer<any, any>> {
	private readonly dmmfModels: readonly DMMF.Model[]

	constructor(context: ObjectTypeGeneratorContext) {
		super(context.schemaComposer, context.options, context.errorHandler, context.attributeProcessor, context.typeMapper)
		this.dmmfModels = context.dmmfModels
	}

	generate(): void {
		try {
			for (const dmmfModel of this.dmmfModels) {
				if (this.shouldGenerateModel(dmmfModel)) {
					this.generateObjectType(dmmfModel)
				}
			}
		} catch (error) {
			this.handleError('generate', error, ['Check model definitions in your schema', 'Ensure field types are valid GraphQL types', 'Verify model attributes are properly configured'])
		}
	}

	getGeneratedTypes(): string[] {
		return this.getGeneratedItems()
	}

	hasObjectType(name: string): boolean {
		return this.hasItem(name)
	}

	getObjectTypeComposer(name: string): ObjectTypeComposer | undefined {
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer instanceof ObjectTypeComposer ? composer : undefined
	}

	getObjectTypeFields(typeName: string): string[] {
		const objectComposer = this.getObjectTypeComposer(typeName)
		return objectComposer ? Object.keys(objectComposer.getFields()) : []
	}

	hasField(typeName: string, fieldName: string): boolean {
		return this.getObjectTypeFields(typeName).includes(fieldName)
	}

	getFieldType(typeName: string, fieldName: string): string | undefined {
		const objectComposer = this.getObjectTypeComposer(typeName)
		if (!objectComposer) {
			return undefined
		}

		const field = objectComposer.getField(fieldName)
		return field?.type?.toString()
	}

	private shouldGenerateModel(dmmfModel: DMMF.Model): boolean {
		return ValidationUtils.shouldGenerateModel(dmmfModel, this.attributeProcessor)
	}

	private generateObjectType(dmmfModel: DMMF.Model): void {
		try {
			const typeName = this.getObjectTypeName(dmmfModel)

			if (this.hasItem(typeName)) {
				return
			}

			const fields = this.createObjectFields(dmmfModel)
			const description = this.getObjectTypeDescription(dmmfModel)

			const objectComposer = this.schemaComposer.createObjectTC({
				name: typeName,
				description,
				fields,
			})

			this.schemaComposer.set(typeName, objectComposer)
			this.registerItem(typeName)
		} catch (error) {
			this.handleError('generateObjectType', error, [`Check model definition for "${dmmfModel.name}"`, 'Ensure model name is a valid GraphQL identifier', 'Verify all field types are supported'])
		}
	}

	private getObjectTypeName(dmmfModel: DMMF.Model): string {
		const customName = ValidationUtils.getModelName(dmmfModel, this.attributeProcessor)
		return this.formatTypeName(customName || dmmfModel.name)
	}

	private getObjectTypeDescription(dmmfModel: DMMF.Model): string | undefined {
		return ValidationUtils.getModelDescription(dmmfModel, this.attributeProcessor)
	}

	private createObjectFields(dmmfModel: DMMF.Model): Record<string, FieldConfig> {
		const fields: Record<string, FieldConfig> = {}

		for (const field of dmmfModel.fields) {
			if (this.shouldIncludeField(dmmfModel, field)) {
				const fieldName = this.getFieldName(dmmfModel, field)
				const fieldConfig = this.createFieldConfig(field)
				fields[fieldName] = fieldConfig
			}
		}

		return fields
	}

	private shouldIncludeField(dmmfModel: DMMF.Model, field: DMMF.Field): boolean {
		return ValidationUtils.shouldIncludeField(dmmfModel, field, this.attributeProcessor, this.options.includeRelations)
	}

	private getFieldName(dmmfModel: DMMF.Model, field: DMMF.Field): string {
		const customName = ValidationUtils.getFieldName(dmmfModel, field.name, this.attributeProcessor)
		return this.formatFieldName(customName || field.name)
	}

	private createFieldConfig(field: DMMF.Field): FieldConfig {
		const graphqlType = this.mapFieldType(field)
		const description = field.documentation

		return {
			type: graphqlType,
			...(description && { description }),
		}
	}

	private mapFieldType(field: DMMF.Field): string {
		if (!this.typeMapper) {
			throw this.handleError('mapFieldType', 'TypeMapper is not initialized')
		}

		if (this.typeMapper.isRelationField(field)) {
			return this.typeMapper.getRelationFieldType(field)
		}

		const mappedType = this.typeMapper.mapFieldType(field)
		if (!mappedType) {
			throw this.handleError('mapFieldType', `Unsupported field type: ${field.type}`, [
				'Check if the field type is supported',
				'Add custom scalar mapping in options',
				'Consider using a different field type',
			])
		}

		return mappedType
	}
}
