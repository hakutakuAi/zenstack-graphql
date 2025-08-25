import { DataModel } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'
import { SortFieldDefinition } from '@generators/strategies'

export class UnifiedSortInputGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		if (this.options.generateSorts) {
			this.outputStrategy.createSortDirectionEnum()
		}
	}

	override generate(): string[] {
		if (!this.options.generateSorts) {
			return []
		}

		return super.generate()
	}

	protected override generateForModel(model: DataModel): string | null {
		const sortFields = this.getSortableFields(model)

		// Get the custom name (or model name) and pass raw name to output strategy
		// Let the output strategy handle the formatting via TypeFactories
		const customName = this.attributeProcessor.model(model).name()
		const sortInputTypeName = this.outputStrategy.createSortInputType(customName, sortFields)

		return sortInputTypeName
	}

	protected override processResults(results: string[]): string[] {
		return this.outputStrategy.getGeneratedTypeNames((name) => name.endsWith('SortInput') || name === 'SortDirection')
	}

	private getSortableFields(model: DataModel): SortFieldDefinition[] {
		const validFields = this.getValidSortableFields(model)

		const sortFields = validFields.map((field) => ({
			name: this.typeFormatter.formatFieldName(field.name),
			description: `Sort by ${field.name}`,
		}))

		if (sortFields.length === 0) {
			sortFields.push({
				name: '_placeholder',
				description: 'Placeholder field when no sortable fields are available',
			})
		}

		return sortFields
	}
}
