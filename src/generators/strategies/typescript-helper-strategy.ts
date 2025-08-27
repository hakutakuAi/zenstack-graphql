import { ModelHelper, HelperGenerationContext } from '@generators/unified/unified-helper-generator'
import { CONNECTION_BUILDER_TEMPLATE, MODEL_CONNECTION_METHOD_TEMPLATE } from '@utils/helper-templates/connection-builder.template'
import { FILTER_BUILDER_TEMPLATE, MODEL_FILTER_METHOD_TEMPLATE } from '@utils/helper-templates/filter-builder.template'
import { SORT_BUILDER_TEMPLATE, MODEL_SORT_METHOD_TEMPLATE } from '@utils/helper-templates/sort-builder.template'
import {
	FIELD_SELECTION_TEMPLATE,
	MODEL_FIELD_SELECTION_METHOD_TEMPLATE,
	INCLUDES_TEMPLATE,
	MODEL_INCLUDE_TEMPLATE,
	RELATION_INCLUDE_TEMPLATE,
} from '@utils/helper-templates/field-selection.template'

export class TypeScriptHelperStrategy {
	generateHelpers(helpers: ModelHelper[], context: HelperGenerationContext): string[] {
		const helperCode = this.buildHelperCode(helpers, context)
		return [helperCode]
	}

	private buildHelperCode(helpers: ModelHelper[], context: HelperGenerationContext): string {
		const imports = this.generateImports(context)
		const connectionBuilder = this.generateConnectionBuilder(helpers, context)
		const filterBuilder = this.generateFilterBuilder(helpers, context)
		const sortBuilder = this.generateSortBuilder(helpers, context)
		const fieldSelection = this.generateFieldSelection(helpers)
		const includes = this.generateIncludes(helpers)

		return `${imports}

${connectionBuilder}

${filterBuilder}

${sortBuilder}

${fieldSelection}

${includes}`
	}

	private generateImports(context: HelperGenerationContext): string {
		const typeImports = context.models
			.filter((model) => !context.attributeProcessor.model(model).isIgnored())
			.flatMap((model) => {
				const graphqlName = context.attributeProcessor.model(model).name()
				const imports = [graphqlName, `${graphqlName}QueryArgs`, `${graphqlName}Connection`, `${graphqlName}FilterInput`]

				if (
					this.shouldGenerateSortForModel(
						{
							modelName: graphqlName,
							relations: [],
							connectionBuilderName: '',
							filterBuilderName: '',
							sortBuilderName: '',
							fieldSelectionName: '',
							includesConstName: '',
						},
						context,
					)
				) {
					imports.push(`${graphqlName}SortInput`)
				}

				return imports
			})
			.join(', ')

		return `import type { GraphQLResolveInfo } from 'graphql'
import { ${typeImports} } from '${this.getSchemaImportPath(context)}'

export interface PaginationArgs {
	first?: number
	after?: string
	last?: number
	before?: string
}

export interface ConnectionResult<T> {
	pageInfo: {
		hasNextPage: boolean
		hasPreviousPage: boolean
		startCursor?: string
		endCursor?: string
	}
	edges: Array<{
		node: T
		cursor: string
	}>
	totalCount: number
}

export interface ConnectionConfig {
	findManyOptions: {
		take: number
		where?: any
		orderBy?: any
		include?: any
		cursor?: any
		skip?: number
	}
	countOptions: {
		where?: any
	}
	paginationInfo: {
		first?: number
		last?: number
		after?: string
		before?: string
		cursorField: string
		hasIdField: boolean
		relationFields: string[]
	}
}`
	}

	private generateConnectionBuilder(helpers: ModelHelper[], context: HelperGenerationContext): string {
		const modelMethods = helpers.map((helper) => this.generateConnectionMethod(helper, context)).join('\n')

		return CONNECTION_BUILDER_TEMPLATE.replace('{{MODEL_SPECIFIC_METHODS}}', modelMethods)
	}

	private generateConnectionMethod(helper: ModelHelper, context: HelperGenerationContext): string {
		const relationFields = helper.relations.map((rel) => `'${rel.fieldName}'`).join(', ')
		const model = this.findModelByGraphQLName(helper.modelName, context)
		const hasIdField = this.modelHasIdField(model)
		const prismaModelName = model ? this.getPrismaModelName(model) : helper.modelName.toLowerCase()
		const cursorField = this.getCursorField(model)

		const hasFilter = this.shouldGenerateFilterForModel(helper, context)
		const hasSort = this.shouldGenerateSortForModel(helper, context)

		let filterSortLogic = ''
		if (hasFilter && hasSort) {
			filterSortLogic = `const where = 'filter' in args ? FilterBuilder.build${helper.modelName}Filter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.build${helper.modelName}Sort((args as any).sort) : undefined`
		} else if (hasFilter) {
			filterSortLogic = `const where = 'filter' in args ? FilterBuilder.build${helper.modelName}Filter((args as any).filter) : {}
		const orderBy = undefined`
		} else if (hasSort) {
			filterSortLogic = `const where = {}
		const orderBy = 'sort' in args ? SortBuilder.build${helper.modelName}Sort((args as any).sort) : undefined`
		} else {
			filterSortLogic = `const where = {}
		const orderBy = undefined`
		}

		return MODEL_CONNECTION_METHOD_TEMPLATE.replace(/{{MODEL_NAME}}/g, helper.modelName)
			.replace(/{{MODEL_NAME_UPPER}}/g, helper.modelName.toUpperCase())
			.replace(/{{PRISMA_MODEL_NAME}}/g, prismaModelName)
			.replace(/{{RELATION_FIELDS}}/g, relationFields)
			.replace(/{{HAS_ID_FIELD}}/g, String(hasIdField))
			.replace(/{{CURSOR_FIELD}}/g, cursorField)
			.replace(/{{FILTER_SORT_LOGIC}}/g, filterSortLogic)
	}

	private generateFilterBuilder(helpers: ModelHelper[], context: HelperGenerationContext): string {
		const modelMethods = helpers.map((helper) => this.generateFilterMethod(helper, context)).join('\n')

		return FILTER_BUILDER_TEMPLATE.replace('{{MODEL_SPECIFIC_METHODS}}', modelMethods)
	}

	private generateFilterMethod(helper: ModelHelper, context: HelperGenerationContext): string {
		if (!this.shouldGenerateFilterForModel(helper, context)) {
			return ''
		}

		return MODEL_FILTER_METHOD_TEMPLATE.replace(/{{MODEL_NAME}}/g, helper.modelName)
	}

	private shouldGenerateFilterForModel(helper: ModelHelper, context: HelperGenerationContext): boolean {
		const model = this.findModelByGraphQLName(helper.modelName, context)
		if (!model) return false

		return model.fields.some((field: any) => context.attributeProcessor.model(model).field(field.name).isFilterable())
	}

	private generateSortBuilder(helpers: ModelHelper[], context: HelperGenerationContext): string {
		const modelMethods = helpers.map((helper) => this.generateSortMethod(helper, context)).join('\n')

		return SORT_BUILDER_TEMPLATE.replace('{{MODEL_SPECIFIC_METHODS}}', modelMethods)
	}

	private generateSortMethod(helper: ModelHelper, context: HelperGenerationContext): string {
		if (!this.shouldGenerateSortForModel(helper, context)) {
			return ''
		}

		const model = this.findModelByGraphQLName(helper.modelName, context)
		const hasIdField = this.modelHasIdField(model)

		return MODEL_SORT_METHOD_TEMPLATE.replace(/{{MODEL_NAME}}/g, helper.modelName).replace(/{{HAS_ID_FIELD}}/g, String(hasIdField))
	}

	private shouldGenerateSortForModel(helper: ModelHelper, context: HelperGenerationContext): boolean {
		const model = this.findModelByGraphQLName(helper.modelName, context)
		if (!model) return false

		return model.fields.some((field: any) => context.attributeProcessor.model(model).field(field.name).isSortable())
	}

	private generateFieldSelection(helpers: ModelHelper[]): string {
		const modelMethods = helpers.map((helper) => this.generateFieldSelectionMethod(helper)).join('\n')

		return FIELD_SELECTION_TEMPLATE.replace('{{MODEL_SPECIFIC_METHODS}}', modelMethods)
	}

	private generateFieldSelectionMethod(helper: ModelHelper): string {
		const relationFields = helper.relations.map((rel) => `'${rel.fieldName}'`).join(', ')

		return MODEL_FIELD_SELECTION_METHOD_TEMPLATE.replace(/{{MODEL_NAME}}/g, helper.modelName).replace(/{{RELATION_FIELDS}}/g, relationFields)
	}

	private generateIncludes(helpers: ModelHelper[]): string {
		const modelIncludes = helpers.map((helper) => this.generateModelInclude(helper)).join('\n')

		return INCLUDES_TEMPLATE.replace('{{MODEL_INCLUDES}}', modelIncludes)
	}

	private generateModelInclude(helper: ModelHelper): string {
		const relationIncludes = helper.relations.map((rel) => RELATION_INCLUDE_TEMPLATE.replace(/{{FIELD_NAME}}/g, rel.fieldName)).join(',\n\t')

		return MODEL_INCLUDE_TEMPLATE.replace(/{{MODEL_NAME_UPPER}}/g, helper.modelName.toUpperCase()).replace(/{{RELATION_INCLUDES}}/g, relationIncludes)
	}

	private findModelByGraphQLName(graphqlName: string, context: HelperGenerationContext): any {
		return context.models.find((model) => context.attributeProcessor.model(model).name() === graphqlName)
	}

	private modelHasIdField(model: any): boolean {
		if (!model) return true

		return model.fields.some((field: any) => field.name === 'id' && !field.type.reference)
	}

	private getPrismaModelName(model: any): string {
		return model.name.toLowerCase()
	}

	private getCursorField(model: any): string {
		if (!model) return 'id'

		const idField = model.fields.find((field: any) => field.name === 'id' && !field.type.reference)

		if (idField) return 'id'

		return 'id'
	}

	private getSchemaImportPath(context: HelperGenerationContext): string {
		// Determine the correct import path based on where the schema.ts file is generated
		// Default to './schema' but can be customized based on output configuration
		return './schema'
	}
}