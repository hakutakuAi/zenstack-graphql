import { DataModel } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'

export class UnifiedConnectionGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		if (this.options.connectionTypes) {
			this.outputStrategy.createPaginationTypes()
		}
	}

	override generate(): string[] {
		if (!this.options.connectionTypes) {
			return []
		}

		return super.generate()
	}

	protected override generateForModel(model: DataModel): string | null {
		// Get the custom name (or model name) and pass raw name to output strategy
		// Let the output strategy handle the formatting via TypeFactories
		const customName = this.attributeProcessor.model(model).name()
		const connectionName = this.outputStrategy.createConnectionType(customName)

		return connectionName
	}

	protected override processResults(results: string[]): string[] {
		return this.outputStrategy.getGeneratedTypeNames((name) => name.endsWith('Connection'))
	}

	protected override createCommonTypes(): void {}
}
