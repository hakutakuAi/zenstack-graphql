import { BaseGenerator } from '@generators/base-generator'
import { DataModel } from '@zenstackhq/sdk/ast'
import { TypeScriptGeneratorContext } from './typescript-generator-factory'

export class TypeScriptConnectionGenerator extends BaseGenerator {
	private astFactory: TypeScriptGeneratorContext['astFactory']
	private createdPageInfo = false

	constructor(context: TypeScriptGeneratorContext) {
		super(context)
		this.astFactory = context.astFactory
	}

	generate(): string[] {
		this.createCommonTypes()
		this.forEachValidModel((model) => this.generateConnectionType(model))
		return []
	}

	private createCommonTypes(): void {
		this.astFactory.createPaginationInputTypes()
	}

	private generateConnectionType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		this.astFactory.createConnectionType(typeName)
	}
}