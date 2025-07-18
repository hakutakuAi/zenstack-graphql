import { ObjectTypeComposer, SchemaComposer } from 'graphql-compose'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { BaseGenerator } from '@generators/base-generator'
import { ErrorCategory } from '@utils/error/error-handler'
import { GeneratorContext } from '@types'
import { ValidationUtils } from '@utils/schema/validation'
import { Generate, SchemaOp } from '@utils/error'

export interface RelationField {
	modelName: string
	fieldName: string
	targetModelName: string
	targetFieldName: string | null
	isList: boolean
	isRequired: boolean
}

export class RelationGenerator extends BaseGenerator {
	private dmmfModels: readonly DMMF.Model[]

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.dmmfModels) {
			throw new Error('DMMF models are required for RelationGenerator')
		}
		if (!context.typeMapper) {
			throw new Error('TypeMapper is required for RelationGenerator')
		}
		this.dmmfModels = context.dmmfModels
	}

	protected override skipGeneration(): boolean {
		return !this.options.includeRelations
	}

	@Generate({
		suggestions: ['Check model relationships in your schema', 'Ensure relation fields are properly defined', 'Verify model types exist in the schema'],
	})
	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		const relations = this.extractRelations()
		relations.forEach((relation) => this.processRelation(relation))
	}

	getGeneratedRelations(): string[] {
		return this.registry.getProcessedRelations()
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

	@SchemaOp({
		suggestions: ['Check relation field definitions', 'Ensure both source and target models exist', 'Verify relation names are consistent'],
	})
	private processRelation(relation: RelationField): void {
		const relationKey = this.getRelationKey(relation)

		if (this.registry.hasProcessedRelation(relationKey)) {
			return
		}

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

		this.registry.addProcessedRelation(relationKey)
	}

	private getRelationKey(relation: RelationField): string {
		return `${relation.modelName}.${relation.fieldName}->${relation.targetModelName}${relation.targetFieldName ? `.${relation.targetFieldName}` : ''}`
	}

	private getRelationFieldType(relation: RelationField): string {
		const typeName = this.typeFormatter.formatTypeName(relation.targetModelName)

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
		const typeName = this.typeFormatter.formatTypeName(modelName)

		if (this.schemaComposer.has(typeName)) {
			const composer = this.schemaComposer.get(typeName)
			if (composer instanceof ObjectTypeComposer) {
				return composer
			}
		}

		return undefined
	}
}
