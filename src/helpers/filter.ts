import type { FilterInput } from './types'

export function buildFilter(filter: FilterInput | null | undefined): any {
	if (!filter || typeof filter !== 'object') return undefined

	return processFilter(filter)
}

function processFilter(filter: FilterInput): any {
	const where: any = {}

	for (const [field, value] of Object.entries(filter)) {
		if (value === undefined || value === null) continue

		if (field === 'AND' && Array.isArray(value)) {
			where.AND = value.map(processFilter)
		} else if (field === 'OR' && Array.isArray(value)) {
			where.OR = value.map(processFilter)
		} else if (field === 'NOT' && typeof value === 'object') {
			where.NOT = processFilter(value)
		} else if (typeof value === 'object' && !Array.isArray(value)) {
			where[field] = processFieldFilter(value)
		} else {
			where[field] = { equals: value }
		}
	}

	return Object.keys(where).length > 0 ? where : undefined
}

function processFieldFilter(fieldValue: any): any {
	const fieldWhere: any = {}

	for (const [operation, operationValue] of Object.entries(fieldValue)) {
		if (operationValue === undefined || operationValue === null) continue

		switch (operation) {
			case 'equals':
			case 'not':
			case 'lt':
			case 'lte':
			case 'gt':
			case 'gte':
				fieldWhere[operation] = operationValue
				break
			case 'contains':
			case 'startsWith':
			case 'endsWith':
				if (typeof operationValue === 'string') {
					fieldWhere[operation] = operationValue
				}
				break
			case 'in':
			case 'notIn':
				if (Array.isArray(operationValue) && operationValue.length > 0) {
					fieldWhere[operation] = operationValue
				}
				break
			default:
				fieldWhere[operation] = operationValue
		}
	}

	return Object.keys(fieldWhere).length > 0 ? fieldWhere : undefined
}
