import type { GraphQLResolveInfo } from 'graphql'
import { buildFilter } from './filter'
import { buildSort } from './sort'
import { buildSelect } from './field-selection'
import type { FilterInput, SortInput, PaginationArgs } from './types'

export class QueryBuilder<TSelect = any> {
	private _where?: any
	private _orderBy?: any
	private _select?: TSelect
	private _pagination?: PaginationArgs

	where(filter: FilterInput | null | undefined): this {
		this._where = buildFilter(filter)
		return this
	}

	orderBy(sort: SortInput | null | undefined, fallback?: any): this {
		this._orderBy = buildSort(sort, fallback)
		return this
	}

	paginate(args: PaginationArgs): this {
		this._pagination = args
		return this
	}

	select(baseSelect: TSelect, info?: GraphQLResolveInfo): this {
		this._select = buildSelect(baseSelect, info)
		return this
	}

	build() {
		const take = this._pagination?.first || this._pagination?.last || 10
		const cursor = this.buildCursor()
		const skip = cursor ? 1 : 0

		return {
			findMany: {
				take: Math.abs(take) + 1,
				where: this._where,
				orderBy: this._orderBy,
				select: this._select,
				...(cursor && { cursor, skip }),
			},
			count: {
				where: this._where,
			},
		}
	}

	private buildCursor(): any {
		const cursorValue = this._pagination?.after || this._pagination?.before
		if (!cursorValue) return undefined

		if (cursorValue.includes(':')) {
			const parts = cursorValue.split(':')
			const field = parts[0] as string
			const value = parts[1]
			return { [field]: value }
		}

		return { id: cursorValue }
	}
}
