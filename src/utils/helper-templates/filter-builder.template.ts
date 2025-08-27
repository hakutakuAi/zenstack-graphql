export const FILTER_BUILDER_TEMPLATE = `export class FilterBuilder {
	static buildFilter(filter: any): any {
		if (!filter || typeof filter !== 'object') return {}

		const where: any = {}

		for (const [field, value] of Object.entries(filter)) {
			if (field === 'AND' && Array.isArray(value)) {
				where.AND = value.map((f: any) => this.buildFilter(f))
			} else if (field === 'OR' && Array.isArray(value)) {
				where.OR = value.map((f: any) => this.buildFilter(f))
			} else if (value && typeof value === 'object') {
				const fieldWhere: any = {}
				
				for (const [operation, operationValue] of Object.entries(value)) {
					if (operationValue !== undefined && operationValue !== null) {
						fieldWhere[operation] = operationValue
					}
				}
				
				if (Object.keys(fieldWhere).length > 0) {
					where[field] = fieldWhere
				}
			}
		}

		return where
	}

	{{MODEL_SPECIFIC_METHODS}}
}`

export const MODEL_FILTER_METHOD_TEMPLATE = `
	static build{{MODEL_NAME}}Filter(filter?: {{MODEL_NAME}}FilterInput): any {
		return this.buildFilter(filter)
	}`

export const FIELD_FILTER_TEMPLATE = ''
export const STRING_FILTER_OPERATIONS = ''
export const NUMERIC_FILTER_OPERATIONS = ''
export const BOOLEAN_FILTER_OPERATIONS = ''
export const DATETIME_FILTER_OPERATIONS = ''