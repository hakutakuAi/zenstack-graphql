import { BaseGenerator } from '@generators/base-generator'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { TypeScriptGeneratorContext } from './typescript-generator-factory'

export class TypeScriptFilterInputGenerator extends BaseGenerator {
	private astFactory: TypeScriptGeneratorContext['astFactory']

	constructor(context: TypeScriptGeneratorContext) {
		super(context)
		this.astFactory = context.astFactory
	}

	generate(): string[] {
		this.createCommonFilterTypes()
		this.forEachValidModel((model) => this.generateFilterInputType(model))
		return []
	}

	private createCommonFilterTypes(): void {
		this.createNumericFilterType()
		this.createDateTimeFilterType()
		this.createStringFilterType()
		this.createBooleanFilterType()
	}

	private createNumericFilterType(): void {
		this.astFactory.createFilterInputType('NumericFilterInput', [
			{ name: 'equals', type: 'Float', nullable: true },
			{ name: 'not', type: 'Float', nullable: true },
			{ name: 'gt', type: 'Float', nullable: true },
			{ name: 'lt', type: 'Float', nullable: true },
		])
	}

	private createDateTimeFilterType(): void {
		this.astFactory.createFilterInputType('DateTimeFilterInput', [
			{ name: 'equals', type: 'Date', nullable: true },
			{ name: 'not', type: 'Date', nullable: true },
			{ name: 'gt', type: 'Date', nullable: true },
			{ name: 'lt', type: 'Date', nullable: true },
		])
	}

	private createStringFilterType(): void {
		this.astFactory.createFilterInputType('StringFilterInput', [
			{ name: 'equals', type: 'String', nullable: true },
			{ name: 'not', type: 'String', nullable: true },
			{ name: 'in', type: '[String!]', nullable: true },
			{ name: 'notIn', type: '[String!]', nullable: true },
			{ name: 'contains', type: 'String', nullable: true },
			{ name: 'startsWith', type: 'String', nullable: true },
			{ name: 'endsWith', type: 'String', nullable: true },
		])
	}

	private createBooleanFilterType(): void {
		this.astFactory.createFilterInputType('BooleanFilterInput', [
			{ name: 'equals', type: 'Boolean', nullable: true },
			{ name: 'not', type: 'Boolean', nullable: true },
		])
	}

	private generateFilterInputType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const filterInputName = this.typeFormatter.formatTypeName(`${typeName}FilterInput`)

		const filterFields = this.getFilterableFields(model)

		filterFields.push(
			{ name: 'AND', type: `[${filterInputName}!]`, nullable: true },
			{ name: 'OR', type: `[${filterInputName}!]`, nullable: true }
		)

		this.astFactory.createFilterInputType(filterInputName, filterFields)
	}

	private getFilterableFields(model: DataModel): Array<{ name: string, type: string, nullable?: boolean }> {
		return model.fields
			.filter((field) => {
				const fieldProcessor = this.attributeProcessor.field(model, field.name)
				const isFilterable = fieldProcessor.isFilterable()
				const shouldInclude = fieldProcessor.shouldInclude(true)
				return isFilterable && shouldInclude && !this.typeMapper?.isRelationField(field)
			})
			.map((field) => ({
				name: this.typeFormatter.formatFieldName(field.name),
				type: this.getFilterInputTypeForField(field),
				nullable: true
			}))
	}

	private getFilterInputTypeForField(field: DataModelField): string {
		switch (field.type.type) {
			case 'String':
			case 'Bytes':
				return 'StringFilterInput'
			case 'Int':
			case 'BigInt':
			case 'Float':
			case 'Decimal':
				return 'NumericFilterInput'
			case 'Boolean':
				return 'BooleanFilterInput'
			case 'DateTime':
				return 'DateTimeFilterInput'
			default:
				return 'StringFilterInput'
		}
	}
}