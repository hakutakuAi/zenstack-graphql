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
			const formattedTypeName = this.getFormattedTypeName(model)
			const queryArgsName = `${formattedTypeName}QueryArgs`
			const description = `Query arguments for ${formattedTypeName} with optional filter, sort, and pagination`

			return this.outputStrategy.createQueryArgsInputType(queryArgsName, model, description)
		} catch (error) {
			const formattedTypeName = this.getFormattedTypeName(model)
			warning(
				`Failed to create query args input for model ${formattedTypeName}: ${error instanceof Error ? error.message : String(error)}`,
				ErrorCategory.GENERATION,
				{
					modelName: formattedTypeName,
					error: error,
				},
			)
			return null
		}
	}
}
