import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { RelationField } from './unified-relation-generator'
import { OutputFormat } from '@utils/constants'

export interface HelperGenerationContext {
	models: DataModel[]
	relations: RelationField[]
	outputFormat: OutputFormat
	attributeProcessor: any
}

export interface ModelHelper {
	modelName: string
	connectionBuilderName: string
	filterBuilderName: string
	sortBuilderName: string
	fieldSelectionName: string
	includesConstName: string
	relations: RelationField[]
}

export class UnifiedHelperGenerator extends UnifiedGeneratorBase {
	constructor(context: UnifiedGeneratorContext) {
		super(context)
	}

	override generate(): string[] {
		const helpers = this.generateHelpers()
		return helpers
	}

	protected override generateForModel(_model: DataModel): string | null {
		return null
	}

	private generateHelpers(): string[] {
		const modelHelpers = this.extractModelHelpers()
		const helperContext: HelperGenerationContext = {
			models: this.models,
			relations: this.extractRelations(),
			outputFormat: this.options.outputFormat,
			attributeProcessor: this.attributeProcessor,
		}

		return this.outputStrategy.generateHelpers?.(modelHelpers, helperContext) || []
	}

	private extractModelHelpers(): ModelHelper[] {
		return this.models
			.filter((model) => !this.shouldSkipModel(model))
			.map((model) => {
				const graphqlName = this.attributeProcessor.model(model).name()
				return {
					modelName: graphqlName,
					connectionBuilderName: `${graphqlName}ConnectionBuilder`,
					filterBuilderName: `${graphqlName}FilterBuilder`,
					sortBuilderName: `${graphqlName}SortBuilder`,
					fieldSelectionName: `${graphqlName}FieldSelection`,
					includesConstName: `${graphqlName.toUpperCase()}_INCLUDES`,
					relations: this.getModelRelations(model),
				}
			})
	}

	private extractRelations(): RelationField[] {
		const relations: RelationField[] = []

		for (const model of this.models) {
			if (this.shouldSkipModel(model)) {
				continue
			}

			for (const field of model.fields) {
				if (this.typeMapper?.isRelationField(field)) {
					const targetModelName = field.type.reference?.ref?.name || ''
					const targetModel = this.findModelByName(targetModelName)

					if (targetModel) {
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
		}

		return relations
	}

	private getModelRelations(model: DataModel): RelationField[] {
		return model.fields
			.filter((field) => this.typeMapper?.isRelationField(field))
			.map((field) => {
				const targetModelName = field.type.reference?.ref?.name || ''
				const targetModel = this.findModelByName(targetModelName)
				const targetField = targetModel ? this.findRelatedField(targetModel, model.name) : null

				return {
					modelName: model.name,
					fieldName: field.name,
					targetModelName,
					targetFieldName: targetField?.name || null,
					isList: field.type.array,
					isRequired: field.type.optional === false,
				}
			})
			.filter((relation) => !!this.findModelByName(relation.targetModelName))
	}

	private findModelByName(name: string): DataModel | undefined {
		return this.models.find((model) => model.name === name)
	}

	private findRelatedField(targetModel: DataModel, sourceModelName: string): DataModelField | undefined {
		return targetModel.fields.find((field) => {
			return this.typeMapper?.isRelationField(field) && field.type.reference?.ref?.name === sourceModelName
		})
	}

	protected override shouldSkipModel(model: DataModel): boolean {
		return this.attributeProcessor.model(model).isIgnored()
	}
}