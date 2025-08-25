import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, warning } from '@utils/error'

export class UnifiedInputGenerator extends UnifiedGeneratorBase {
	constructor(context: UnifiedGeneratorContext) {
		super(context)
	}

	protected generateForModel(model: DataModel): string | null {
		const results: string[] = []

		const createInput = this.generateCreateInput(model)
		const updateInput = this.generateUpdateInput(model)

		if (createInput) results.push(createInput)
		if (updateInput) results.push(updateInput)

		return results.length > 0 ? results.join('\n') : null
	}

	private generateCreateInput(model: DataModel): string | null {
		try {
			const typeName = this.getFormattedTypeName(model)
			// Get the custom name or use the model name, then format it properly
			const customName = this.attributeProcessor.model(model).name()
			const inputName = this.typeFormatter.formatCreateInputTypeName(customName)
			const description = `Create input for ${typeName}`

			return this.outputStrategy.createInputType(inputName, model, 'create', description)
		} catch (error) {
			warning(
				`Failed to create create input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
				ErrorCategory.GENERATION,
				{
					modelName: model.name,
					error: error,
				},
			)
			return null
		}
	}

	private generateUpdateInput(model: DataModel): string | null {
		try {
			const typeName = this.getFormattedTypeName(model)
			// Get the custom name or use the model name, then format it properly
			const customName = this.attributeProcessor.model(model).name()
			const inputName = this.typeFormatter.formatUpdateInputTypeName(customName)
			const description = `Update input for ${typeName}`

			return this.outputStrategy.createInputType(inputName, model, 'update', description)
		} catch (error) {
			warning(
				`Failed to create update input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
				ErrorCategory.GENERATION,
				{
					modelName: model.name,
					error: error,
				},
			)
			return null
		}
	}
}
