import { BaseGenerator } from '@generators/base-generator'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError, warning } from '@utils/error'

export class ObjectGenerator extends BaseGenerator {
	private astFactory: TypeScriptASTFactory

	constructor(context: any) {
		super(context)
		this.astFactory = new TypeScriptASTFactory(this.typeFormatter)
	}

	generate(): string[] {
		const results: string[] = []
		this.forEachValidModel((model) => {
			const result = this.generateObjectType(model)
			if (result) {
				results.push(result)
			}
		})
		return results
	}

	private generateObjectType(model: DataModel): string | null {
		try {
			const classDeclaration = this.astFactory.createObjectType(model)
			return classDeclaration.getFullText()
		} catch (error) {
			if (error instanceof PluginError) {
				warning(`Failed to create TypeGraphQL object type for model ${model.name}: ${error.message}`, error.category, {
					modelName: model.name,
					error: error,
				})
			} else {
				warning(
					`Failed to create TypeGraphQL object type for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
					ErrorCategory.GENERATION,
					{
						modelName: model.name,
						error: error,
					},
				)
			}
			return null
		}
	}
}
