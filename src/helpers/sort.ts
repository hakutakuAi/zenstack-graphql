import type { SortInput } from './types'

export function buildSort(sort: SortInput | null | undefined, fallback: any = { id: 'asc' }): any {
	if (!sort || typeof sort !== 'object') return fallback

	const orderBy = processSort(sort)
	return Object.keys(orderBy).length > 0 ? orderBy : fallback
}

function processSort(sort: SortInput): any {
	const orderBy: any = {}

	for (const [field, direction] of Object.entries(sort)) {
		if (field.startsWith('_placeholder')) {
			continue
		}

		if (typeof direction === 'string') {
			const normalizedDirection = direction.toLowerCase()
			if (['asc', 'desc'].includes(normalizedDirection)) {
				orderBy[field] = normalizedDirection
			}
		} else if (typeof direction === 'object' && direction !== null) {
			orderBy[field] = processNestedSort(direction)
		}
	}

	return orderBy
}

function processNestedSort(nestedSort: any): any {
	if (typeof nestedSort !== 'object' || nestedSort === null) return 'asc'

	const processed: any = {}
	for (const [key, value] of Object.entries(nestedSort)) {
		if (typeof value === 'string' && ['asc', 'desc'].includes(value.toLowerCase())) {
			processed[key] = value.toLowerCase()
		}
	}

	return Object.keys(processed).length > 0 ? processed : 'asc'
}
