import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'
import { FilterFieldDefinition } from '@generators/strategies'

export class UnifiedFilterInputGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		this.outputStrategy.createCommonFilterTypes()
	}

	protected override generateForModel(model: DataModel): string | null {
		const typeName = this.getFormattedTypeName(model)
		const filterFields = this.getFilterableFields(model)

		const filterInputName = this.outputStrategy.createFilterInputType(typeName, filterFields)

		return this.typeFormatter.formatFilterInputTypeName(typeName)
	}

	protected override processResults(results: string[]): string[] {
		return this.outputStrategy.getGeneratedTypeNames((name) => name.endsWith('FilterInput'))
	}

	private getFilterableFields(model: DataModel): FilterFieldDefinition[] {
		const validFields = this.getValidFilterableFields(model)

		return validFields
			.filter((field) => !this.typeMapper?.isRelationField(field))
			.map((field) => ({
				name: this.typeFormatter.formatFieldName(field.name),
				type: this.getFilterInputTypeForField(field),
				nullable: true,
				description: `Filter by ${field.name}`,
			}))
	}

	private getFilterInputTypeForField(field: DataModelField): string {
		const fieldProcessor = this.attributeProcessor.field(this.models.find((m) => m.fields.includes(field))!, field.name)

		if (fieldProcessor.isRangeFilterableType()) {
			if (field.type.type === 'DateTime') {
				const dateTimeType = this.options.scalarTypes?.['DateTime'] || 'DateTime'
				return `${dateTimeType}FilterInput`
			} else {
				return 'NumericFilterInput'
			}
		} else if (fieldProcessor.isStringSearchableType()) {
			return 'StringFilterInput'
		} else if (field.type.type === 'Boolean') {
			return 'BooleanFilterInput'
		} else {
			return 'StringFilterInput'
		}
	}
	protected override getValidFilterableFields(model: DataModel) {
		return model.fields.filter((field) => {
			const fieldProcessor = this.attributeProcessor.field(model, field.name)
			return fieldProcessor.isFilterable() && fieldProcessor.shouldInclude(true)
		})
	}
}
