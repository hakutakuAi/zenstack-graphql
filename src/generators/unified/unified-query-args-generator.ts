import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, warning } from '@utils/error'

export class UnifiedQueryArgsGenerator extends UnifiedGeneratorBase {
	constructor(context: UnifiedGeneratorContext) {
		super(context)
	}

	protected generateForModel(model: DataModel): string | null {
		try {
			const queryArgsName = `${model.name}QueryArgs`
			const description = `Query arguments for ${model.name} with optional filter, sort, and pagination`

			return this.outputStrategy.createQueryArgsInputType(queryArgsName, model, description)
		} catch (error) {
			warning(
				`Failed to create query args input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
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