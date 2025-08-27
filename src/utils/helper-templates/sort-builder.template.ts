export const SORT_BUILDER_TEMPLATE = `export class SortBuilder {
	/**
	 * Build Prisma orderBy clause from GraphQL sort input dynamically
	 * This approach uses runtime reflection to map sort fields
	 */
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
				// Convert enum values to lowercase for Prisma
				orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	{{MODEL_SPECIFIC_METHODS}}
}`

export const MODEL_SORT_METHOD_TEMPLATE = `
	static build{{MODEL_NAME}}Sort(sort?: {{MODEL_NAME}}SortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = {{HAS_ID_FIELD}} ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}`

// Remove deprecated template
export const FIELD_SORT_TEMPLATE = ''