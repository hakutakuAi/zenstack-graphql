import { SchemaComposer, ObjectTypeComposer } from 'graphql-compose'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { BaseGenerator } from '@generators/base-generator'
import { AttributeProcessor } from '@utils/attribute-processor'
import { ErrorHandler, ErrorCategory } from '@utils/error-handler'
import { TypeMapper } from '@utils/type-mapper'
import { NormalizedOptions } from '@utils/options-validator'
import { ValidationUtils } from '@utils/validation'

export interface RelationGeneratorContext {
	schemaComposer: SchemaComposer
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	typeMapper: TypeMapper
	dmmfModels: readonly DMMF.Model[]
}

export interface RelationField {
	modelName: string
	fieldName: string
	targetModelName: string
	targetFieldName: string | null
	isList: boolean
	isRequired: boolean
}

export class RelationGenerator extends BaseGenerator<ObjectTypeComposer<any, any>> {
	private dmmfModels: readonly DMMF.Model[]
	private processedRelations: Set<string> = new Set()

	constructor(context: RelationGeneratorContext) {
		super(context.schemaComposer, context.options, context.errorHandler, context.attributeProcessor, context.typeMapper)
		this.dmmfModels = context.dmmfModels
	}

	generate(): void {
		this.generateRelations()
	}

	generateRelations(): void {
		if (!this.options.includeRelations) {
			return
		}

		try {
			const relations = this.extractRelations()

			for (const relation of relations) {
				this.processRelation(relation)
			}
		} catch (error) {
			this.handleError('generateRelations', error, ['Check model relationships in your schema', 'Ensure relation fields are properly defined', 'Verify model types exist in the schema'])
		}
	}

	getProcessedRelations(): string[] {
		return Array.from(this.processedRelations)
	}

	private extractRelations(): RelationField[] {
		const relations: RelationField[] = []

		for (const model of this.dmmfModels) {
			if (this.shouldSkipModel(model)) {
				continue
			}

			for (const field of model.fields) {
				if (this.typeMapper?.isRelationField(field)) {
					if (this.shouldSkipField(model, field)) {
						continue
					}

					const targetModel = this.findModelByName(field.type)

					if (!targetModel) {
						this.errorHandler.logWarning(`Target model ${field.type} not found for relation ${model.name}.${field.name}`, ErrorCategory.SCHEMA, {
							modelName: model.name,
							fieldName: field.name,
							targetModelName: field.type,
						})
						continue
					}

					const targetField = this.findRelatedField(targetModel, model.name, field.relationName)

					relations.push({
						modelName: model.name,
						fieldName: field.name,
						targetModelName: field.type,
						targetFieldName: targetField?.name || null,
						isList: field.isList,
						isRequired: field.isRequired,
					})
				}
			}
		}

		return relations
	}

	private processRelation(relation: RelationField): void {
		const relationKey = this.getRelationKey(relation)

		if (this.processedRelations.has(relationKey)) {
			return
		}

		try {
			const sourceType = this.getObjectTypeComposer(relation.modelName)
			const targetType = this.getObjectTypeComposer(relation.targetModelName)

			if (!sourceType || !targetType) {
				this.errorHandler.logWarning(`Cannot process relation ${relationKey}: object types not found`, ErrorCategory.SCHEMA, { relation })
				return
			}

			if (!sourceType.hasField(relation.fieldName)) {
				const fieldType = this.getRelationFieldType(relation)
				const fieldDescription = this.getRelationFieldDescription(relation)

				sourceType.addFields({
					[relation.fieldName]: {
						type: fieldType,
						description: fieldDescription,
					},
				})
			}

			if (relation.targetFieldName && !targetType.hasField(relation.targetFieldName)) {
				const backReferenceRelation = this.createBackReferenceRelation(relation)
				const fieldType = this.getRelationFieldType(backReferenceRelation)
				const fieldDescription = this.getRelationFieldDescription(backReferenceRelation)

				targetType.addFields({
					[relation.targetFieldName]: {
						type: fieldType,
						description: fieldDescription,
					},
				})
			}

			this.processedRelations.add(relationKey)
		} catch (error) {
			this.handleError('processRelation', error, ['Check relation field definitions', 'Ensure both source and target models exist', 'Verify relation names are consistent'])
		}
	}

	private getRelationKey(relation: RelationField): string {
		return `${relation.modelName}.${relation.fieldName}->${relation.targetModelName}${relation.targetFieldName ? `.${relation.targetFieldName}` : ''}`
	}

	private getRelationFieldType(relation: RelationField): string {
		const typeName = this.formatTypeName(relation.targetModelName)

		if (relation.isList) {
			return `[${typeName}!]!`
		}

		return relation.isRequired ? `${typeName}!` : typeName
	}

	private getRelationFieldDescription(relation: RelationField): string | undefined {
		return `Relation to ${relation.targetModelName}${relation.targetFieldName ? ` via ${relation.targetFieldName}` : ''}`
	}

	private createBackReferenceRelation(relation: RelationField): RelationField {
		if (!relation.targetFieldName) {
			throw new Error(`Cannot create back reference for relation without target field: ${relation.modelName}.${relation.fieldName}`)
		}

		return {
			modelName: relation.targetModelName,
			fieldName: relation.targetFieldName,
			targetModelName: relation.modelName,
			targetFieldName: relation.fieldName,
			isList: !relation.isList,
			isRequired: false,
		}
	}

	private shouldSkipModel(model: DMMF.Model): boolean {
		return ValidationUtils.shouldGenerateModel(model, this.attributeProcessor) === false
	}

	private shouldSkipField(model: DMMF.Model, field: DMMF.Field): boolean {
		return ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, true) === false
	}

	private findModelByName(name: string): DMMF.Model | undefined {
		return this.dmmfModels.find((model) => model.name === name)
	}

	private findRelatedField(model: DMMF.Model, relatedModelName: string, relationName?: string): DMMF.Field | undefined {
		return model.fields.find((field) => field.type === relatedModelName && field.relationName === relationName)
	}

	private getObjectTypeComposer(modelName: string): ObjectTypeComposer | undefined {
		const typeName = this.formatTypeName(modelName)

		if (this.schemaComposer.has(typeName)) {
			const composer = this.schemaComposer.get(typeName)
			if (composer instanceof ObjectTypeComposer) {
				return composer
			}
		}

		return undefined
	}
}
