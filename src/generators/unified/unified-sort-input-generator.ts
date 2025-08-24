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
		const typeName = this.getFormattedTypeName(model)
		const sortFields = this.getSortableFields(model)

		const sortInputTypeName = this.typeFormatter.formatSortInputTypeName(typeName)
		this.outputStrategy.createSortInputType(sortInputTypeName, sortFields)

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
