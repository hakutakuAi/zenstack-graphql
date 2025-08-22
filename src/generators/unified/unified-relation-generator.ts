import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError, warning } from '@utils/error'

export interface RelationField {
	modelName: string
	fieldName: string
	targetModelName: string
	targetFieldName: string | null
	isList: boolean
	isRequired: boolean
}

export class UnifiedRelationGenerator extends UnifiedGeneratorBase {
	constructor(context: UnifiedGeneratorContext) {
		super(context)
	}

	override generate(): string[] {
		const relations = this.extractRelations()
		relations.forEach((relation) => this.processRelation(relation))
		return []
	}

	protected override generateForModel(model: DataModel): string | null {
		return null
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
						warning(`Target model ${targetModelName} not found for relation ${model.name}.${field.name}`, ErrorCategory.SCHEMA, {
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
		try {
			this.outputStrategy.processRelation(relation)
		} catch (error) {
			if (error instanceof PluginError) {
				warning(error.message, error.category, { relation })
			} else {
				warning(`Failed to process relation: ${relation.modelName}.${relation.fieldName}`, ErrorCategory.SCHEMA, {
					relation,
					error,
				})
			}
		}
	}

	private getRelationKey(relation: RelationField): string {
		return `${relation.modelName}.${relation.fieldName}->${relation.targetModelName}${relation.targetFieldName ? `.${relation.targetFieldName}` : ''}`
	}

	protected override shouldSkipModel(model: DataModel): boolean {
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
}
