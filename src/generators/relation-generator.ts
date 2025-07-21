import { ObjectTypeComposer } from 'graphql-compose'
import { Result, ok, err } from 'neverthrow'
import { BaseGenerator } from '@generators/base-generator'
import { ErrorCategory, logWarning } from '@utils/error'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'

export interface RelationField {
	modelName: string
	fieldName: string
	targetModelName: string
	targetFieldName: string | null
	isList: boolean
	isRequired: boolean
}

export class RelationGenerator extends BaseGenerator {
	protected override skipGeneration(): boolean {
		return !this.options.includeRelations
	}

	generate(): string[] {
		if (this.skipGeneration()) {
			return []
		}

		const relations = this.extractRelations()
		relations.forEach((relation) => this.processRelation(relation))
		return this.registry.getProcessedRelations()
	}

	private extractRelations(): RelationField[] {
		const relations: RelationField[] = []

		for (const model of this.models) {
			if (this.shouldSkipModel(model)) {
				continue
			}

			for (const field of model.fields) {
				if (this.typeMapper?.isRelationField(field)) {
					if (this.shouldSkipField(model, field)) {
						continue
					}

					const targetModelName = field.type.reference?.ref?.name || ''
					const targetModel = this.findModelByName(targetModelName)

					if (!targetModel) {
						logWarning(`Target model ${targetModelName} not found for relation ${model.name}.${field.name}`, ErrorCategory.SCHEMA, {
							modelName: model.name,
							fieldName: field.name,
							targetModelName,
						})
						continue
					}

					const targetField = this.findRelatedField(targetModel, model.name)

					relations.push({
						modelName: model.name,
						fieldName: field.name,
						targetModelName,
						targetFieldName: targetField?.name || null,
						isList: field.type.array,
						isRequired: field.type.optional === false,
					})
				}
			}
		}

		return relations
	}

	private processRelation(relation: RelationField): void {
		const relationKey = this.getRelationKey(relation)

		if (this.registry.hasProcessedRelation(relationKey)) {
			return
		}

		const sourceType = this.getObjectTypeComposer(relation.modelName)
		const targetType = this.getObjectTypeComposer(relation.targetModelName)

		if (!sourceType || !targetType) {
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
			const backReferenceResult = this.createBackReferenceRelation(relation)
			if (backReferenceResult.isErr()) {
				logWarning(backReferenceResult.error, ErrorCategory.SCHEMA, { relation })
				return
			}

			const backReferenceRelation = backReferenceResult.value
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
		const targetModel = this.findModelByName(relation.targetModelName)

		const typeName = targetModel ? this.attributeProcessor.model(targetModel).getFormattedTypeName(this.typeFormatter) : this.typeFormatter.formatTypeName(relation.targetModelName)

		if (relation.isList) {
			return `[${typeName}!]!`
		}

		return relation.isRequired ? `${typeName}!` : typeName
	}

	private getRelationFieldDescription(relation: RelationField): string | undefined {
		return `Relation to ${relation.targetModelName}${relation.targetFieldName ? ` via ${relation.targetFieldName}` : ''}`
	}

	private createBackReferenceRelation(relation: RelationField): Result<RelationField, string> {
		if (!relation.targetFieldName) {
			return err(`Cannot create back reference for relation without target field: ${relation.modelName}.${relation.fieldName}`)
		}

		return ok({
			modelName: relation.targetModelName,
			fieldName: relation.targetFieldName,
			targetModelName: relation.modelName,
			targetFieldName: relation.fieldName,
			isList: !relation.isList,
			isRequired: false,
		})
	}

	private shouldSkipModel(model: DataModel): boolean {
		return this.attributeProcessor.model(model).isIgnored()
	}

	private shouldSkipField(model: DataModel, field: DataModelField): boolean {
		return !this.attributeProcessor.field(model, field.name).shouldInclude(true)
	}

	private findModelByName(name: string): DataModel | undefined {
		return this.models.find((model) => model.name === name)
	}

	private findRelatedField(model: DataModel, sourceModelName: string): DataModelField | undefined {
		return model.fields.find((field) => {
			if (!field.type.reference || !field.type.reference.ref) return false

			return field.type.reference.ref.name === sourceModelName
		})
	}

	private getObjectTypeComposer(modelName: string): ObjectTypeComposer | undefined {
		const model = this.findModelByName(modelName)

		const typeName = model ? this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter) : this.typeFormatter.formatTypeName(modelName)

		if (this.schemaComposer.has(typeName)) {
			const composer = this.schemaComposer.get(typeName)
			if (composer instanceof ObjectTypeComposer) {
				return composer
			}
		}

		return undefined
	}
}
