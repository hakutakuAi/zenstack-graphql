import { Resolver, Query, Mutation, Arg, Ctx, ID } from 'type-graphql'
import { Product, ProductCreateInput, ProductUpdateInput, ProductQueryArgs, ProductConnection } from '../../schema'
import { Context } from './types'

@Resolver(() => Product)
export class ProductResolver {
	@Query(() => Product, { nullable: true })
	async product(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Product | null> {
		return ctx.prisma.product.findUnique({
			where: { id },
			include: {
				reviews: true,
				tags: {
					include: {
						tag: true,
					},
				},
			},
		})
	}

	@Query(() => ProductConnection)
	async products(@Arg('args', { nullable: true }) args: ProductQueryArgs, @Ctx() ctx: Context): Promise<ProductConnection> {
		const take = args?.first || 20
		const skip = args?.after ? 1 : 0
		const cursor = args?.after ? { id: args.after } : undefined

		const products = await ctx.prisma.product.findMany({
			take: take + 1,
			skip,
			cursor,
			where: this.buildWhereCondition(args?.filter),
			orderBy: this.buildOrderBy(args?.sort),
			include: {
				reviews: true,
				tags: {
					include: {
						tag: true,
					},
				},
			},
		})

		const hasNextPage = products.length > take
		const nodes = hasNextPage ? products.slice(0, -1) : products

		const edges = nodes.map((product, index) => ({
			node: product,
			cursor: product.id,
		}))

		return {
			edges,
			pageInfo: {
				hasNextPage,
				hasPreviousPage: false,
				startCursor: edges[0]?.cursor,
				endCursor: edges[edges.length - 1]?.cursor,
			},
			totalCount: await ctx.prisma.product.count({
				where: this.buildWhereCondition(args?.filter),
			}),
		}
	}

	@Mutation(() => Product)
	async createProduct(@Arg('input') input: ProductCreateInput, @Ctx() ctx: Context): Promise<Product> {
		return ctx.prisma.product.create({
			data: input,
			include: {
				reviews: true,
				tags: {
					include: {
						tag: true,
					},
				},
			},
		})
	}

	@Mutation(() => Product)
	async updateProduct(@Arg('id', () => ID) id: string, @Arg('input') input: ProductUpdateInput, @Ctx() ctx: Context): Promise<Product> {
		return ctx.prisma.product.update({
			where: { id },
			data: input,
			include: {
				reviews: true,
				tags: {
					include: {
						tag: true,
					},
				},
			},
		})
	}

	@Mutation(() => Boolean)
	async deleteProduct(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.product.delete({
			where: { id },
		})
		return true
	}

	private buildWhereCondition(filter: any) {
		if (!filter) return undefined

		const where: any = {}

		if (filter.name) {
			where.name = { contains: filter.name.contains || filter.name.equals }
		}

		if (filter.price) {
			where.price = {}
			if (filter.price.gte !== undefined) where.price.gte = filter.price.gte
			if (filter.price.lte !== undefined) where.price.lte = filter.price.lte
			if (filter.price.equals !== undefined) where.price.equals = filter.price.equals
		}

		if (filter.status) {
			where.status = filter.status.equals || filter.status
		}

		if (filter.createdAt) {
			where.createdAt = {}
			if (filter.createdAt.gte) where.createdAt.gte = filter.createdAt.gte
			if (filter.createdAt.lte) where.createdAt.lte = filter.createdAt.lte
		}

		// Handle AND/OR operations
		if (filter.AND) {
			where.AND = filter.AND.map((f: any) => this.buildWhereCondition(f))
		}

		if (filter.OR) {
			where.OR = filter.OR.map((f: any) => this.buildWhereCondition(f))
		}

		return where
	}

	private buildOrderBy(sort: any) {
		if (!sort) return { createdAt: 'desc' }

		const orderBy: any = {}

		if (sort.name) orderBy.name = sort.name.toLowerCase()
		if (sort.price) orderBy.price = sort.price.toLowerCase()
		if (sort.createdAt) orderBy.createdAt = sort.createdAt.toLowerCase()

		return orderBy
	}
}
