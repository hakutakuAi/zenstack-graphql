import { BaseGenerator } from '@generators/base-generator'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, warning } from '@utils/error'

export class InputGenerator extends BaseGenerator {
	private astFactory: TypeScriptASTFactory

	constructor(context: any) {
		super(context)
		this.astFactory = new TypeScriptASTFactory(this.typeFormatter)
	}

	generate(): string[] {
		const results: string[] = []
		this.forEachValidModel((model) => {
			const createInput = this.generateCreateInput(model)
			const updateInput = this.generateUpdateInput(model)
			if (createInput) results.push(createInput)
			if (updateInput) results.push(updateInput)
		})
		return results
	}

	private generateCreateInput(model: DataModel): string | null {
		try {
			const classDeclaration = this.astFactory.createInputType(model, 'CreateInput')
			return classDeclaration.getFullText()
		} catch (error) {
			warning(
				`Failed to create TypeScript create input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
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
			const classDeclaration = this.astFactory.createInputType(model, 'UpdateInput')
			return classDeclaration.getFullText()
		} catch (error) {
			warning(
				`Failed to create TypeScript update input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`,
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
