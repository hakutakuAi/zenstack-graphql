import { GeneratorContext } from '@types'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Generate, SchemaOp, Validate } from '@utils/error'

export class FilterInputGenerator extends BaseGenerator {
	private models: DataModel[]
	private typeFactories: GraphQLTypeFactories

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.models) {
			throw new Error('Models are required for FilterInputGenerator')
		}
		if (!context.typeMapper) {
			throw new Error('TypeMapper is required for FilterInputGenerator')
		}
		this.models = context.models
		this.typeFactories = new GraphQLTypeFactories(this.schemaComposer, this.errorHandler, this.typeFormatter)
	}

	protected override skipGeneration(): boolean {
		return !this.options.connectionTypes
	}

	@Generate({
		suggestions: ['Check model definitions in your schema', 'Ensure filter input types are properly configured'],
	})
	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.createCommonFilterTypes()

		this.models.filter((model) => ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)).forEach((model) => this.generateFilterInputType(model))
	}

	@SchemaOp()
	private createCommonFilterTypes(): void {
		if (!this.schemaComposer.has('NumericFilterInput')) {
			const numericFilterTC = this.schemaComposer.createInputTC({
				name: 'NumericFilterInput',
				description: 'Input type for numeric filtering operations',
				fields: {
					equals: {
						type: 'Float',
						description: 'Equal to the given value',
					},
					not: {
						type: 'Float',
						description: 'Not equal to the given value',
					},
					gt: {
						type: 'Float',
						description: 'Greater than the given value',
					},
					lt: {
						type: 'Float',
						description: 'Less than the given value',
					},
				},
			})
			this.registry.registerType('NumericFilterInput', TypeKind.INPUT, numericFilterTC, true)
		}

		if (!this.schemaComposer.has('DateTimeFilterInput')) {
			const dateFilterTC = this.schemaComposer.createInputTC({
				name: 'DateTimeFilterInput',
				description: 'Input type for datetime filtering operations',
				fields: {
					equals: {
						type: 'DateTime',
						description: 'Equal to the given value',
					},
					not: {
						type: 'DateTime',
						description: 'Not equal to the given value',
					},
					gt: {
						type: 'DateTime',
						description: 'Greater than the given value',
					},
					lt: {
						type: 'DateTime',
						description: 'Less than the given value',
					},
				},
			})
			this.registry.registerType('DateTimeFilterInput', TypeKind.INPUT, dateFilterTC, true)
		}

		if (!this.schemaComposer.has('StringFilterInput')) {
			const stringFilterTC = this.schemaComposer.createInputTC({
				name: 'StringFilterInput',
				description: 'Input type for string filtering operations',
				fields: {
					equals: {
						type: 'String',
						description: 'Equal to the given value',
					},
					not: {
						type: 'String',
						description: 'Not equal to the given value',
					},
					in: {
						type: '[String!]',
						description: 'In the given list of values',
					},
					notIn: {
						type: '[String!]',
						description: 'Not in the given list of values',
					},
					contains: {
						type: 'String',
						description: 'Contains the given value',
					},
					startsWith: {
						type: 'String',
						description: 'Starts with the given value',
					},
					endsWith: {
						type: 'String',
						description: 'Ends with the given value',
					},
				},
			})
			this.registry.registerType('StringFilterInput', TypeKind.INPUT, stringFilterTC, true)
		}

		if (!this.schemaComposer.has('BooleanFilterInput')) {
			const booleanFilterTC = this.schemaComposer.createInputTC({
				name: 'BooleanFilterInput',
				description: 'Input type for boolean filtering operations',
				fields: {
					equals: {
						type: 'Boolean',
						description: 'Equal to the given value',
					},
					not: {
						type: 'Boolean',
						description: 'Not equal to the given value',
					},
				},
			})
			this.registry.registerType('BooleanFilterInput', TypeKind.INPUT, booleanFilterTC, true)
		}
	}

	@SchemaOp({
		suggestions: ['Check field definitions in the model', 'Ensure fields are valid for filtering'],
	})
	private generateFilterInputType(model: DataModel): void {
		const typeName = this.getObjectTypeName(model)
		const filterInputName = this.typeFormatter.formatTypeName(`${typeName}FilterInput`)

		if (this.schemaComposer.has(filterInputName)) {
			return
		}

		const fields: Record<string, { type: string; description: string }> = {}

		model.fields
			.filter((field) => this.isFilterableField(model, field))
			.forEach((field) => {
				const fieldName = this.typeFormatter.formatFieldName(field.name)

				let filterType: string

				if (ValidationUtils.isRangeFilterableType(field)) {
					if (field.type.type === 'DateTime') {
						filterType = 'DateTimeFilterInput'
					} else {
						filterType = 'NumericFilterInput'
					}
				} else if (ValidationUtils.isStringSearchableType(field)) {
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

	@Validate()
	private isFilterableField(model: DataModel, field: DataModelField): boolean {
		return ValidationUtils.isFieldFilterable(model, field.name, this.attributeProcessor) && ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, true)
	}

	private getObjectTypeName(model: DataModel): string {
		const customName = ValidationUtils.getModelName(model, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || model.name)
	}

	getGeneratedFilterInputTypes(): string[] {
		return this.registry.getTypesByKind(TypeKind.INPUT).filter((name) => name.endsWith('FilterInput'))
	}

	hasFilterInputType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.INPUT) && name.endsWith('FilterInput')
	}

	getFilterInputTypeForModel(modelName: string): string | undefined {
		const filterInputName = `${this.typeFormatter.formatTypeName(modelName)}FilterInput`
		return this.registry.isTypeOfKind(filterInputName, TypeKind.INPUT) ? filterInputName : undefined
	}
}
