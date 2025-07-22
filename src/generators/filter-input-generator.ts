import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@/utils/registry/registry'

export class FilterInputGenerator extends BaseGenerator {
	generate(): string[] {
		this.createCommonFilterTypes()

		this.models.filter((model) => !this.attributeProcessor.model(model).isIgnored()).forEach((model) => this.generateFilterInputType(model))

		return this.registry.getTypesByKind(TypeKind.INPUT).filter((name) => name.endsWith('FilterInput'))
	}

	private createCommonFilterTypes(): void {
		this.createNumericFilterType()
		this.createDateTimeFilterType()
		this.createStringFilterType()
		this.createBooleanFilterType()
	}

	private createNumericFilterType(): void {
		this.createFilterType('NumericFilterInput', 'numeric', 'Float', false)
	}

	private createDateTimeFilterType(): void {
		const dateTimeType = this.options.scalarTypes['DateTime'] || 'DateTime'
		this.createFilterType(`${dateTimeType}FilterInput`, 'datetime', dateTimeType, false)
	}

	private createStringFilterType(): void {
		this.createFilterType('StringFilterInput', 'string', 'String', true)
	}

	private createBooleanFilterType(): void {
		this.createFilterType('BooleanFilterInput', 'boolean', 'Boolean', false)
	}

	private createFilterType(name: string, description: string, typeName: string, includeStringOperations: boolean): void {
		if (this.schemaComposer.has(name)) {
			return
		}

		const fields: Record<string, { type: string; description: string }> = {
			equals: {
				type: typeName,
				description: 'Equal to the given value',
			},
			not: {
				type: typeName,
				description: 'Not equal to the given value',
			},
		}

		if (typeName !== 'Boolean' && typeName !== 'String') {
			fields.gt = {
				type: typeName,
				description: 'Greater than the given value',
			}
			fields.lt = {
				type: typeName,
				description: 'Less than the given value',
			}
		}

		if (includeStringOperations) {
			fields.in = {
				type: `[${typeName}!]`,
				description: 'In the given list of values',
			}
			fields.notIn = {
				type: `[${typeName}!]`,
				description: 'Not in the given list of values',
			}
			fields.contains = {
				type: typeName,
				description: 'Contains the given value',
			}
			fields.startsWith = {
				type: typeName,
				description: 'Starts with the given value',
			}
			fields.endsWith = {
				type: typeName,
				description: 'Ends with the given value',
			}
		}

		const filterTC = this.schemaComposer.createInputTC({
			name,
			description: `Input type for ${description} filtering operations`,
			fields,
		})

		this.registry.registerType(name, TypeKind.INPUT, filterTC, true)
	}

	private generateFilterInputType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const filterInputName = this.typeFormatter.formatTypeName(`${typeName}FilterInput`)

		if (this.schemaComposer.has(filterInputName)) {
			return
		}

		const fields: Record<string, { type: string; description: string }> = {}

		model.fields
			.filter((field) => this.isFilterableField(model, field))
			.forEach((field) => {
				const fieldName = this.attributeProcessor.field(model, field.name).getFormattedFieldName(this.typeFormatter)

				let filterType: string

				const fieldProcessor = this.attributeProcessor.field(model, field.name)

				if (fieldProcessor.isRangeFilterableType()) {
					if (field.type.type === 'DateTime') {
						const dateTimeType = this.options.scalarTypes['DateTime'] || 'DateTime'
						filterType = `${dateTimeType}FilterInput`
					} else {
						filterType = 'NumericFilterInput'
					}
				} else if (fieldProcessor.isStringSearchableType()) {
					filterType = 'StringFilterInput'
				} else if (field.type.type === 'Boolean') {
					filterType = 'BooleanFilterInput'
				} else {
					return
				}

				fields[fieldName] = {
					type: filterType,
					description: `Filter by ${fieldName}`,
				}
			})

		if (Object.keys(fields).length === 0) {
			return
		}

		fields.AND = {
			type: `[${filterInputName}!]`,
			description: 'Logical AND operation',
		}

		fields.OR = {
			type: `[${filterInputName}!]`,
			description: 'Logical OR operation',
		}

		const filterInputTC = this.schemaComposer.createInputTC({
			name: filterInputName,
			description: `Filter input type for ${typeName}`,
			fields,
		})

		this.registry.registerType(filterInputName, TypeKind.INPUT, filterInputTC, true)
	}

	protected isFilterableField(model: DataModel, field: DataModelField): boolean {
		const fieldProcessor = this.attributeProcessor.field(model, field.name)
		return fieldProcessor.isFilterable() && fieldProcessor.shouldInclude(true)
	}
}
