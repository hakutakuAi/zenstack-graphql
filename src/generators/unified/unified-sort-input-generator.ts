import { DataModel } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'
import { SortFieldDefinition } from '@generators/strategies'

export class UnifiedSortInputGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		this.outputStrategy.createSortDirectionEnum()
	}

	protected override generateForModel(model: DataModel): string | null {
		const typeName = this.getFormattedTypeName(model)
		const sortFields = this.getSortableFields(model)

		const sortInputName = this.outputStrategy.createSortInputType(typeName, sortFields)

		return this.typeFormatter.formatSortInputTypeName(typeName)
	}

	protected override processResults(results: string[]): string[] {
		return this.outputStrategy.getGeneratedTypeNames((name) => name.endsWith('SortInput'))
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
