import { DataModel } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'

export class UnifiedConnectionGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		this.outputStrategy.createPaginationTypes()
	}

	protected override generateForModel(model: DataModel): string | null {
		const typeName = this.getFormattedTypeName(model)

		const connectionName = this.outputStrategy.createConnectionType(typeName)

		return this.typeFormatter.formatConnectionTypeName(typeName)
	}

	protected override processResults(results: string[]): string[] {
		return this.outputStrategy.getGeneratedTypeNames((name) => name.endsWith('Connection'))
	}

	protected override createCommonTypes(): void {}
}
