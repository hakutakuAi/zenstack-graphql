import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Tag, TagCreateInput, TagUpdateInput, TagConnection, TagFilterInput, TagSortInput, ProductTag } from '../../schema'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import type { Context } from './types'
import { TAG_WITH_PRODUCTS_SELECT, PRODUCT_TAG_WITH_RELATIONS_SELECT } from '../select-definitions'

@Resolver(() => Tag)
export class TagResolver {
	@Query(() => Tag, { nullable: true })
	async tag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Tag | null> {
		return (await ctx.prisma.tag.findUnique({
			where: { id },
			select: TAG_WITH_PRODUCTS_SELECT,
		})) as Tag | null
	}

	@Query(() => TagConnection)
	async tags(
		@Arg('filter', () => TagFilterInput, { nullable: true }) filter: TagFilterInput | undefined,
		@Arg('sort', () => TagSortInput, { nullable: true }) sort: TagSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() ctx: Context,
	): Promise<TagConnection> {
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(TAG_WITH_PRODUCTS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([ctx.prisma.tag.findMany(config.findMany), ctx.prisma.tag.count(config.count)])

		return config.toConnection(items, totalCount) as TagConnection
	}

	@Mutation(() => Tag)
	async createTag(@Arg('input', () => TagCreateInput) input: TagCreateInput, @Ctx() ctx: Context): Promise<Tag> {
		return (await ctx.prisma.tag.create({
			data: input,
			select: TAG_WITH_PRODUCTS_SELECT,
		})) as Tag
	}

	@Mutation(() => Tag)
	async updateTag(@Arg('id', () => ID) id: string, @Arg('input', () => TagUpdateInput) input: TagUpdateInput, @Ctx() ctx: Context): Promise<Tag> {
		return (await ctx.prisma.tag.update({
			where: { id },
			data: input,
			select: TAG_WITH_PRODUCTS_SELECT,
		})) as Tag
	}

	@Mutation(() => Boolean)
	async deleteTag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.tag.delete({ where: { id } })
		return true
	}

	@Mutation(() => Boolean)
	async assignTagToProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.create({ data: { tagId, productId } })
		return true
	}

	@Mutation(() => Boolean)
	async removeTagFromProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.delete({ where: { productId_tagId: { productId, tagId } } })
		return true
	}

	@FieldResolver(() => [ProductTag])
	async products(@Root() tag: Tag, @Ctx() ctx: Context): Promise<ProductTag[]> {
		return (await ctx.prisma.productTag.findMany({
			where: { tagId: tag.id },
			select: PRODUCT_TAG_WITH_RELATIONS_SELECT,
		})) as ProductTag[]
	}
}
