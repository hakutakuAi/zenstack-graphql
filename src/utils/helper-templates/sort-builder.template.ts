export const SORT_BUILDER_TEMPLATE = `export class SortBuilder {
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
					orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	{{MODEL_SPECIFIC_METHODS}}
}`

export const MODEL_SORT_METHOD_TEMPLATE = `
	static build{{MODEL_NAME}}Sort(sort?: {{MODEL_NAME}}SortInput): any {
		const fallbackSort = {{HAS_ID_FIELD}} ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}`

export const FIELD_SORT_TEMPLATE = ''