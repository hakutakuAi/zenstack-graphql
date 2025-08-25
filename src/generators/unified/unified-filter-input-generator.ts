import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { UnifiedGeneratorBase } from './unified-generator-base'
import { FilterFieldDefinition } from '@generators/strategies'

export class UnifiedFilterInputGenerator extends UnifiedGeneratorBase {
	protected override beforeGeneration(): void {
		if (this.options.generateFilters) {
			this.outputStrategy.createCommonFilterTypes()
			this.generateEnumFilterInputs()
		}
	}

	protected override afterGeneration(): void {}

	override generate(): string[] {
		if (!this.options.generateFilters) {
			return []
		}

		return super.generate()
	}

	protected override generateForModel(model: DataModel): string | null {
		const filterFields = this.getFilterableFields(model)

		// Get the custom name (or model name) and pass raw name to output strategy
		// Let the output strategy handle the formatting via TypeFactories
		const customName = this.attributeProcessor.model(model).name()
		
		if (filterFields.length === 0) {
			return this.outputStrategy.createEmptyFilterInputType(customName)
		} else {
			return this.outputStrategy.createFilterInputType(customName, filterFields)
		}
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

		if (field.type.reference?.ref?.name && this.typeMapper?.isEnumType(field.type.reference.ref.name)) {
			return `${field.type.reference.ref.name}FilterInput`
		}

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

	private generateEnumFilterInputs(): void {
		const enumsUsed = new Set<string>()

		this.models.forEach((model) => {
			const filterableFields = this.getValidFilterableFields(model)
			filterableFields.forEach((field) => {
				if (field.type.reference?.ref?.name && this.typeMapper?.isEnumType(field.type.reference.ref.name)) {
					enumsUsed.add(field.type.reference.ref.name)
				}
			})
		})

		enumsUsed.forEach((enumName) => {
			this.outputStrategy.createEnumFilterInputType(enumName)
		})
	}
}
